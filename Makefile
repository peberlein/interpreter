CFLAGS += -O3 -march=native -mtune=native 

ifeq ($(CC),gcc)
  CFLAGS += -fno-gcse -fno-crossjumping
endif

interpret:
