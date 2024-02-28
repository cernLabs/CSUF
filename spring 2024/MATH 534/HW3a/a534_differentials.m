syms b11 b12 b13 b14 b21 b22 b23 b24 b31 b32 b33 b34 b41 b42 b43 b44
syms a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44
syms mu1 mu2 mu3 mu4 n
A = [a11 a12 a13 a14; a21 a22 a23 a24; a31 a32 a33 a34; a41 a42 a43 a44]
dsig = zeros(4);
dsig(1,1) = 1;

A*dsig;

mu = [mu1;mu2;mu3;mu4];

%case when k=1
dmuT = [1 0 0 0];
trace(A*dsig*A*mu*dmuT)

%case when k=2
dmuT = [0 1 0 0];
trace(A*dsig*A*mu*dmuT)

%case when k=3
dmuT = [0 0 1 0];
trace(A*dsig*A*mu*dmuT)

%case when k=4
dmuT = [0 0 0 1];
trace(A*dsig*A*mu*dmuT)



syms b11 b12 b13 b14 b21 b22 b23 b24 b31 b32 b33 b34 b41 b42 b43 b44
syms a11 a12 a13 a14 a21 a22 a23 a24 a31 a32 a33 a34 a41 a42 a43 a44
syms n
A = [a11 a12 a13 a14; a21 a22 a23 a24; a31 a32 a33 a34; a41 a42 a43 a44];
B = [b11 b12 b13 b14; b21 b22 b23 b24; b31 b32 b33 b34; b41 b42 b43 b44];

% i = j = 2, k = l = 4
dsig11 = zeros(4);
dsig12 = zeros(4);
dsig11(2,2) = 1;
dsig12(4,4) = 1;
n*trace(A*dsig12*B*dsig11)

% i = 1, j = 2, k = 3, l = 4
dsig11 = zeros(4);
dsig12 = zeros(4);
dsig11(1,2) = 1;
dsig11(2,1) = 1;
dsig12(3,4) = 1;
dsig12(4,3) = 1;
n*trace(A*dsig12*B*dsig11)

% i = 1, j = 2, k = l = 3
dsig11 = zeros(4);
dsig12 = zeros(4);
dsig11(1,2) = 1;
dsig11(2,1) = 1;
dsig12(3,3) = 1;
n*trace(A*dsig12*B*dsig11)

% i = j = 1, k = 3, l = 4
dsig11 = zeros(4);
dsig12 = zeros(4);
dsig11(1,1) = 1;
dsig12(3,4) = 1;
dsig12(4,3) = 1;
n*trace(A*dsig12*B*dsig11)

