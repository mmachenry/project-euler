#define N 100
#include <stdio.h>
#include <stdlib.h>

const int size = ((N + 1) / sizeof(int)) + 1;

int triangle (int n) {
	return n*(n-1)/2;
}

int position (int a, int b, int c) {
	int pos = 0;
	for (int i = 0; i < a; i++) { pos += triangle((N+1)-i); }
	for (int i = 0; i < b; i++) { pos += ... }
	pos += c;
	return pos;
}

int index (int a, int b, int c) {
	int pos = position(a,b,c);
	/*???*/
	return pos/sizeof(int);
}

int offset (int a, int b, int c) {
	int pos = position(a,b,c);
	/*???*/
	return pos % sizeof(int);
}

void set (int *states, int a, int b, int c) {
	states[index(a,b,c)] |= offset(a,b,c);
}

int get (int *states, int a, int b, int c) {
	return states[index(a,b,c)] & offset(a,b,c);
}

int main (int argc, char** argv) {
	int *states = malloc(size * sizeof(int));
	long long int total = 0;
	
	for (int a = 0; a <= N; a++) {
		for (int b = a; b <= N; b++) {
			for (int c = b; c <= N; c++) {
				if (!get(states, a,b,c)) {
					total += a+b+c;
				}

				for (int i = 1; a+i <= N; i++) {
					set(states, a+i,b,c);
				}
				for (int i = 1; b+i <= N; i++) {
					set(states, a,b+i,c);
				}
				for (int i = 1; c+i <= N; i++) {
					set(states, a,b,c+i);
				}
				for (int i = 1; a+i <= N && b+i <= N; i++) {
					set(states, a+i,b+i,c);
				}
				for (int i = 1; a+i <= N && c+i <= N; i++) {
					set(states, a+i,b,c+i);
				}
				for (int i = 1; b+i <= N && c+i <= N; i++) {
					set(states, a,b+i,c+i);
				}
				for (int i = 1; a+i <= N && b+i <= N && c+i <= N; i++) {
					set(states, a+i,b+i,c+i);
				}
			}
		}
	}

	free(states);
	printf("%lld\n", total);
	return 0;
}

