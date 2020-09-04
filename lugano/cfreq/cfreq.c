#import <stdio.h>
#import <math.h>
#import <limits.h>
#import <ctype.h>

struct pair {
  int c;
  unsigned int freq;
};

int find_len(struct pair *all_pairs) {
  int len = 0;
  for(int i = 0; i < UCHAR_MAX; ++i) {
    len += (all_pairs[i].freq > 0) ? 1 : 0;
  }
  return len;
}

void reduce_pairs(struct pair *all_pairs, struct pair *pairs) {
  int j = 0;
  for(int i = 0; i < UCHAR_MAX; ++i) {
    if (all_pairs[i].freq > 0) {
      pairs[j] = all_pairs[i];
      ++j;
    }
  }
}

void swap(struct pair *pairs, int i, int j) {
  struct pair tmp = pairs[i];
 pairs[i] = pairs[j];
 pairs[j] = tmp;  
}

// Quicksort in descending order
void quickSort(struct pair *pairs, int start, int end) {
  if (end <= start) { return; }

  unsigned int pivot = pairs[start].freq;
  int i = start + 1;
  int j = end;
  while (1) {
    while (pairs[i].freq > pivot && i < end) {
      ++i;
    }
    while (pairs[j].freq <= pivot && j > start) {
      --j;
    }

    if (i >= j) break;

    swap(pairs, i, j);
  }
  
  swap(pairs, start, j);
  quickSort(pairs, start, j - 1);
  quickSort(pairs, j + 1, end);
}

int main(void) {
  static struct pair all_pairs[UCHAR_MAX];
  int c;
  
  while ((c = getchar()) != EOF) {
    all_pairs[c].c = c;
    ++all_pairs[c].freq;
  }

  // take subset of all_pairs and sort by freq descending
  int len = find_len(all_pairs);
  struct pair pairs[len];
  reduce_pairs(all_pairs, pairs);
  quickSort(pairs, 0, len - 1);
  
  for(int i = 0; i < len; ++i) {
    if (isprint(pairs[i].c))
      printf("'%c' %u\n", pairs[i].c, pairs[i].freq);
    else
      printf("%3o %u\n", pairs[i].c, pairs[i].freq);
  }
  
  return 0;
};
