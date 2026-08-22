# format the raw "<name> <lang> <reps> <ms> <chk>" lines emitted by the
# harnesses into a comparison table. per-iteration time = ms / reps, so the
# auto-scaled rep counts cancel out. one "<lang> ms/it" column is printed per
# language present in the data, in the order given by -v langs="..."; "ok" flags
# whether every language's checksum for that bench agrees.
BEGIN { nl = split(langs, L, " ") }
{
  # skip anything that isn't a well-formed result line (5 fields, positive reps);
  # tolerates empty / partial / garbage files from a skipped or broken run.
  if (NF != 5 || $3 + 0 <= 0) next
  name = $1; lang = $2; reps = $3; ms = $4; chk = $5
  if (!(name in seen)) { order[++n] = name; seen[name] = 1 }
  per[name, lang] = ms / reps
  csum[name, lang] = chk
  have[name, lang] = 1
  langhas[lang] = 1
}
END {
  # keep only languages that actually produced rows, preserving -v langs order.
  nc = 0
  for (j = 1; j <= nl; j++) if (L[j] in langhas) cols[++nc] = L[j]

  # size every numeric column to the widest header so long labels (e.g.
  # "luajit-nojit ms/it") don't collide with their neighbour.
  cw = 14
  for (j = 1; j <= nc; j++) { hl = length(cols[j]) + 8; if (hl > cw) cw = hl }
  w = 12; for (j = 1; j <= nc; j++) w += cw
  bar = ""; for (i = 0; i < w + 5; i++) bar = bar "-"

  printf "%-12s", "bench"
  for (j = 1; j <= nc; j++) printf "%*s", cw, cols[j] " ms/it"
  printf "%5s\n%s\n", "ok", bar

  for (i = 1; i <= n; i++) {
    b = order[i]
    printf "%-12s", b
    for (j = 1; j <= nc; j++)
      printf "%*s", cw, have[b, cols[j]] ? sprintf("%.4f", per[b, cols[j]]) : "-"
    base = ""; ok = "yes"
    for (j = 1; j <= nc; j++)
      if (have[b, cols[j]]) {
        if (base == "") base = csum[b, cols[j]]
        else if (csum[b, cols[j]] != base) ok = "NO"
      }
    printf "%5s\n", ok
  }
}
