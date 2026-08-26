#!/bin/sh
# test/gate/asrefuse.sh -- the OTHER half of the as.l gate. a golden can only say what the bytes
# ARE; these say that there are none. that is the half which keeps a silent mis-encoding from
# passing for an answer, and the reason it exists: `movq $N, %rax' with an undefined N once
# assembled to `movq $30' -- as-num read the character's value off the front and stopped.
# ⚠ nothing unwinds through a scare, so each case is its own love.
#
# usage: asrefuse.sh LOVE      (from the repo root; LOVE is a word list, not a path)
set -u
love=$1
n=0 leaked=0

try() {   # try NAME SOURCE  -- SOURCE must RAISE
  n=$((n + 1))
  { cat crew/holo/holo.l crew/holo/amd64.l crew/holo/as.l
    printf "(use 'holo)\n(: _ (puts (as-hex \"%s\")) _ 0)\n" "$2"
  } | $love > /dev/null 2>&1 &&
    { echo "FAIL asrefuse: $1 -- assembled, should have raised"; leaked=$((leaked + 1)); }
}

# a number is required and a symbol is not one: the front cannot relocate an immediate, so
# every door that reads one has to say so rather than read a prefix and stop.
try "symbolic immediate"     'movq $N, %rax'
try "symbolic displacement"  'movq sym(%rbx), %rax'
try "immediate with a tail"  'movq $4x, %rax'
try "empty hex"              'movq $0x, %rax'
try "symbolic scale"         'movq %rax, (%rbx,%rcx,n)'
# shapes x86 has no encoding for, or the front no width for
try "no width"               'mov $1, (%rbx)'
try "no width, unary"        'neg (%rbx)'
try "two memory operands"    'movq (%rax), (%rbx)'
try "width clash"            'movq %eax, (%rbx)'
try "immediate destination"  'movq %rax, $1'
try "bad scale"              'movq %rax, (%rbx,%rcx,3)'
try "too many mem fields"    'movq %rax, (%rbx,%rcx,4,8)'
# ⚠ rel32 is measured from the end of its own field and the fixup carries no addend, so a
# rip-relative operand with an immediate tail cannot be spelled -- it must not be guessed.
try "rip with an imm tail"   'movq $1, t(%rip)\nt:\n ret'
# and the plain unknowns
try "unknown mnemonic"       'frobq %rax, %rbx'
try "unknown register"       'movq %rzz, %rbx'

echo "test/holo/asrefuse: $n refused, $leaked leaked"
[ "$leaked" -eq 0 ] || exit 1
