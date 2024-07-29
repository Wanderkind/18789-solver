import std;
import core.memory;

alias Grid = int[14][8];
alias IndexPair = int[2];
alias IPL = IndexPair[];
alias Record = IPL[10];


Grid createInitialGrid() {
    Grid array;
    foreach (ref row; array) {
        foreach (ref elem; row) {
            elem = uniform(0, 10);
        }
    }
    return array;
}

bool adjacent(IndexPair p, IndexPair q) {
    int a = p[0], b = p[1];
    int c = q[0], d = q[1];
    return (abs(a - c) < 2) && (abs(b - d) < 2) && (a != c || b != d);
}

Grid replaceElement(const Grid original, size_t row, size_t col, int newValue) {
    Grid modified = original;
    modified[row][col] = newValue;
    return modified;
}

bool cando(IPL[] lst, size_t index = 0, IndexPair prev = [0, 0]) {
	if (index == lst.length) {
		return true;
	}
	foreach (elem; lst[index]) {
		if (index == 0 || adjacent(elem, prev)) {
			if (cando(lst, index + 1, elem)) {
				return true;
			}
		}
	}
	return false;
}

int score(Grid g) {
    IPL l; int w;
    Record mem = [[], [], [], [], [], [], [], [], [], []];
    foreach(i; 0..8) {
        foreach(j; 0..14) {
            w = g[i][j];
            mem[w] ~= [i, j];
		}
	}
    int sc = 1, ss; IPL[] sl;
    while(true) {
        sl = []; ss = sc;
        while(ss) {
            sl ~= mem[ss % 10];
            ss /= 10;
		}
        if(cando(sl.reverse())) sc++;
		else return sc - 1;
	}
}

void printgrid(Grid g, int sc) {
    foreach(r; g) {
        foreach(c; r) printf("%d", c);
        writeln();
	}
    printf("score = %d\n\n", sc);
}

Grid readgrid() {
    Grid res;
    string line;
    foreach(i; 0..8) {
        line = readln().strip();
        foreach(j; 0..14) {
            res[i][j] = line[j] - '0';
		}
	}
    return res;
}

bool loop(int d, Grid g) {
    Grid currentgrid = g;
    int currentscore = score(g);
    printgrid(g, currentscore);
    int nextscore = currentscore;
    Grid[] warehouse;
    int e, s;
    Grid tempgrid;
    foreach(i; 0..8) {
        foreach(j; 0..14) {
			//if(i == 0 || i == 7 || j < 2 || j > 12) {
				e = currentgrid[i][j];
				foreach(x; 0..10) {
					if(x != e) {
						tempgrid = replaceElement(currentgrid, i, j, x);
						s = score(tempgrid);
						if(s > nextscore) {
							nextscore = s;
							currentgrid = tempgrid;
						}
						if(s == nextscore) warehouse ~= tempgrid;
					}
				}
			//}
		}
	}
    if(nextscore > 5884) {
        writeln("################\n################\n");
        printgrid(currentgrid, nextscore);
        writeln("################\n################");
        return true;
	} else if (nextscore > currentscore) {
        return loop(d, currentgrid);
	} else {
        int len = cast(int)warehouse.length;
        int count = 0;
        foreach(h; warehouse) {
            count++;
            printf("depth %d: searching (%d/%d)...\n", d, count, len);
            foreach(p; 0..8) {
                foreach(q; 0..14) {
                    e = h[p][q];
                    foreach(r; 0..10) {
                        if(r != e) {
                            tempgrid = replaceElement(h, p, q, r);
                            if(score(tempgrid) > nextscore) {
                                writeln();
                                if(loop(d + 1, tempgrid)) return true;
							}
						}
					}
				}
			}
		}
        writeln();
        return false;
	}
}

void main() {
    GC.disable();
	//Grid g = createInitialGrid();
	Grid g = readgrid();
	bool cont = true;
    while(cont) {
        if(loop(0, g)) cont = false;
		g = createInitialGrid();
	}
	readgrid();
}
