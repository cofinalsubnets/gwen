add_executable(hello_serial
        hello_serial.c
        ../../../src/core.c
        ../../../src/equal.c
        ../../../src/eval.c
        ../../../src/hash.c
        ../../../src/mem.c
        ../../../src/num.c
        ../../../src/read.c
        ../../../src/str.c
        ../../../src/sym.c
        ../../../src/thread.c
        ../../../src/two.c
        ../../../src/vm.c
        ../../../src/write.c
        )

include_directories(
  ../../../src
)
# pull in common dependencies
target_link_libraries(hello_serial pico_stdlib)

# create map/bin/hex/uf2 file etc.
pico_add_extra_outputs(hello_serial)

# add url via pico_set_program_url
example_auto_set_url(hello_serial)
