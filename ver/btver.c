#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define WORD_BITS (8 * sizeof(unsigned int))

#define BITSET_SIZE 1003001
#define BITSET_HASH(a, b) ((a * 314159ll + b) % BITSET_SIZE)

static inline void bs_set(unsigned int *bitarray, size_t idx) {
  bitarray[idx / WORD_BITS] |= (1 << (idx % WORD_BITS));
}

static inline int bs_get(unsigned int *bitarray, size_t idx) {
  return (bitarray[idx / WORD_BITS] & (1 << (idx % WORD_BITS)));
}

#define MAX_N ((size_t)200000)

uint64_t g[MAX_N];

int main(int argc, char **argv) {
  if (argc == 1)
    return 0;
  char fname[100];
  sprintf(fname, "./input/barntree/%s.in", argv[1]);
  FILE *input = fopen(fname, "r");

  int n, p, expected_p;
  fscanf(input, "%d\n", &n);
  char *line = NULL;
  size_t len = 0;
  getline(&line, &len, input);
  char *init = strtok(line, " ");
  int i = 1;
  uint64_t tot = 0;

  while (init != NULL) {
    g[i] = atoll(init);
    tot += g[i];
    i++;
    init = strtok(NULL, " ");
  }

  uint64_t avg = tot / n;
  unsigned int *bitarray =
      (unsigned int *)calloc(BITSET_SIZE / 8 + 1, sizeof(unsigned int));

  uint64_t a, b, tmp;
  for (int i = 0; i < n - 1; i++) {
    fscanf(input, "%lld %lld\n", &a, &b);
    if (a > b) {
      tmp = a;
      a = b;
      b = tmp;
    }
    bs_set(bitarray, BITSET_HASH(a, b));
  }

  sprintf(fname, "./input/barntree/%s.out", argv[1]);
  FILE *output = fopen(fname, "r");
  fscanf(output, "%d\n", &expected_p);
  fclose(output);

  scanf("%d\n", &p);
  if (p != expected_p) {
    printf("incorrect n: %d != %d\n", p, expected_p);
    return 1;
  }

  int from, to;
  uint64_t amt;
  for (int i = 0; i < p; i++) {
    scanf("%d %d %llu\n", &from, &to, &amt);

    if (from > to) {
      a = to, b = from;
    } else {
      a = from, b = to;
    }

    if (bs_get(bitarray, BITSET_HASH(a, b)) == 0) {
      printf("nonexistent edge %d %d\n", from, to);
      return 1;
    }

    if (a > b) {
      tmp = a;
      a = b;
      b = tmp;
    }

    if (g[from] < amt) {
      printf("illegal move from %d %d %llu (only %llu)\n", from, to, amt,
             g[from]);
      return 1;
    }

    g[from] -= amt;
    g[to] += amt;
  }

  for (int i = 1; i <= n; i++) {
    if (g[i] != avg) {
      printf("final amount at %d is %llu != %llu\n", i, g[i], avg);
      return 1;
    }
  }

  printf("good!\n");

  fclose(input);
}
