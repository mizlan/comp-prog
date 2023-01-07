#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define MAX_N (2ll * 100000)

typedef struct ev {
  int end;
  uint64_t pos;
} ev;

int cmp(const void *e1, const void *e2) {
  ev f = *(ev *)e1;
  ev s = *(ev *)e2;
  if (f.pos == s.pos)
    return f.end - s.end;
  return f.pos > s.pos ? 1 : -1;
}

ev evs[MAX_N * 2];
uint64_t ret[MAX_N * 2 + 1];

int main() {
  uint64_t n, a, b, prev;
  int i, d = 0;
  scanf("%llu\n", &n);
  for (i = 0; i < n; i++) {
    scanf("%llu %llu\n", &a, &b);
    evs[i * 2] = (ev){0, a};
    evs[i * 2 + 1] = (ev){1, b};
  }
  qsort(evs, 2 * n, sizeof(ev), cmp);
  for (int i = 0; i < 2 * n;) {
    ev e = evs[i];
    if (i != 0) {
      ret[d] += e.pos - prev - 1;
    }
    while (i < 2 * n && evs[i].pos == e.pos && !evs[i].end) {
      i++;
      d++;
    }
    ret[d]++;
    while (i < 2 * n && evs[i].pos == e.pos && evs[i].end) {
      i++;
      d--;
    }
    prev = e.pos;
  }
  for (int i = 1; i < n; i++) {
    printf("%llu ", ret[i]);
  }
  printf("%llu\n", ret[n]);
}
