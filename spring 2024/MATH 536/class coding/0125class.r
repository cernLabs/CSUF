> df <- csv.read('amb_waittime.csv')
Error in csv.read("amb_waittime.csv") : 
  could not find function "csv.read"
> df <- read.csv('amb_waittime.csv')
> df
    Waittime
1       9.31
2       2.07
3      33.37
4      15.77
5       8.92
6       4.14
7      21.21
8      16.98
9      16.46
10      8.42
11     19.08
12     22.30
13     21.33
14      1.78
15     18.20
16      6.49
17     30.12
18      2.10
19      2.37
20     13.23
21      0.93
22     13.64
23     18.26
24      2.08
25      4.21
26     10.73
27      1.31
28      1.60
29     24.34
30      3.83
31     10.86
32      2.95
33      4.03
34      3.43
35     11.44
36     28.76
37     20.61
38      0.12
39     26.60
40     28.96
41      2.61
42      8.70
43      1.29
44      4.20
45      8.55
46     39.55
47     15.70
48      6.94
49      2.34
50      8.13
51     38.37
52      2.03
53      6.63
54      7.01
55      4.57
56      3.17
57      4.14
58      4.80
59      5.60
60      2.49
61      3.96
62      4.09
63      2.42
64     24.18
65      1.36
66     26.75
67     10.45
68      2.62
69      8.54
70     13.22
71      4.94
72      0.38
73      1.97
74      1.48
75      9.76
76      3.27
77      9.60
78     11.12
79      0.19
80      0.22
81      5.45
82      3.75
83      4.90
84     19.57
85     22.58
86      1.21
87     26.23
88     11.13
89      0.84
90     47.24
91     14.99
92     20.42
93      7.02
94      5.42
95     36.91
96      0.79
97      2.75
98     37.77
99      5.83
100    14.86
101     0.19
102    14.23
103     4.44
104    24.59
105     1.06
106     5.23
107     2.27
108    12.75
109     2.26
110     2.04
111     2.53
112     5.28
113     1.05
114     3.66
115     2.56
116     0.98
117     7.51
118     7.29
119    20.91
120    37.57
121     0.63
122    17.15
123     2.74
124     3.63
125     2.88
126     8.85
127    10.89
128     3.02
129     2.32
130     3.60
131     1.32
132     0.76
133    11.66
134     2.24
135     1.72
136    21.68
137    29.98
138     2.70
139     2.82
140    30.36
141    15.52
142    25.58
143     5.45
144    14.55
145    15.42
146     0.06
147    46.03
148     6.28
149     1.97
150     0.32
151    25.07
152     8.62
153     5.45
154    48.63
155    45.53
156     4.47
157    18.71
158     1.38
159     5.21
160     9.01
161     7.80
162     3.76
163     6.53
164     3.59
165    28.26
166     9.63
167     9.32
168     5.21
169    20.10
170     3.54
171     1.81
172    28.24
173     1.25
174     3.77
175     4.07
176     2.65
177     6.85
178     1.79
179    14.97
180     1.60
181    14.25
182    17.16
183     1.42
184    10.76
185    22.72
186    17.99
187     1.54
188     0.75
189     2.21
190    12.02
191     9.53
192     2.04
193     2.57
194     1.56
195    12.88
196     0.19
197    15.80
198     8.43
199     1.29
200    20.98
201     2.36
202     5.44
203     2.32
204    22.93
205     0.48
206     2.68
207     1.38
208     2.93
209     6.50
210    12.82
211     1.78
212     0.24
213    25.37
214    19.81
215     4.07
216     6.78
217     0.92
218    14.39
219     2.98
220    10.58
221     6.94
222     2.93
223    18.28
224    12.39
225     1.66
226    39.90
227     2.11
228     2.12
229    19.55
230    16.72
231     4.00
232     4.84
233    12.29
234    10.61
235     4.26
236     3.94
237     1.22
238     4.53
239     5.53
240     0.42
241     5.39
242     5.95
243    15.05
244     4.92
245     9.78
246     4.31
247    12.59
248     1.35
249     1.67
250     1.01
251     3.08
252     5.15
253    28.15
254     9.09
255    10.79
256     2.11
257    12.22
258     3.30
259    27.95
260    11.26
261     4.42
262    36.84
263    30.10
264    55.67
265    20.14
266     5.08
267     1.99
268     6.30
269    20.74
270    10.99
271     6.33
272     4.94
273    23.76
274    12.57
275     1.87
276    14.40
277     0.42
278    44.45
279     7.64
280     4.57
281     2.75
282    20.29
283     6.00
284    15.90
285     8.73
286     0.49
287     8.69
288    33.56
289     3.77
290    10.64
291    13.61
292     7.53
293    19.85
294    12.82
295     9.20
296     2.89
297    11.39
298     9.60
299     9.21
300    19.48
> # testing to see if you think the true wait time is more than ten minutes
> # nullhyp: wait is 10 minutes
> S = 0
> for (i in 1:300){}
> for (i in 1:300){
+ if (df$Waittime[i] > 10.0){
+ S <- S + 1
+ }
+ else {
+ S <- S + 0
+ }
+ }
> S
[1] 111
> S/300
[1] 0.37
> # I would like to sell my client a p-value
> # Nichols way: Theoretical first
> 
> lenght(df$Waittime)
Error in lenght(df$Waittime) : could not find function "lenght"
> length(df$Waittime)
[1] 300
> mean(df$Waittime)
[1] 10.3284
> sd(df$Waittime)
[1] 10.53669
> #h0: mu = 10, ha: mu > 10
> z = (10-10.3284)/10.53669
> pnorm(z)
[1] 0.4875681
> z = (10.3284-10)/10.53669
> pnorm(z)
[1] 0.5124319
> z = (10.3284-10)*(sqrt(300))/10.53669
> pnorm(z)
[1] 0.705344
> z = (10.3284-10)/(10.53669/sqrt(300))
> pnorm(z)
[1] 0.705344
> pnorm(z,0,1,lower.tail = F)
[1] 0.294656
> pnorm(z, lower.tail = F)
[1] 0.294656
> # now do this same problem in simulation
> ## take random sample from our population (if the mean is equal to 10). for each value in the population subtract true mean then add 10
> ###take a sample of size 300 from our adjusted population with replacement, record sample mean
> #### repeat step 2, 10000 times, record all 10000 sample means
> ##### histograms are our friends (should look roughly normal)
> ###### empirically compute what percentage of our simulated means under the h0 are greater than the original xbar of 10.32
> # but... I don't have the population. step1 is doomed
> # solution: create a pseudo-population (one that will have similar shape and variance characteristics to the original population but will have a different mean... that's okay, we adjust the mean anyway, no biggie).
> # use the original sample with infinite replication of its observations as your new pseudo population  (bootstrapping). Our bootstrap population with have similar variance and shape to the true population but will be centered at the original xbar instead of the true mu
> ## take random sample from our ****pseudo/bootstrap population (if the mean is equal to 10). for each value in the population subtract true mean then add 10
> BS.pop = df$Waittime
> BS.pop = df$Waittime - mean(df$Waittime) + 10
> sample.samp = sample(BS.pop,300,replace=T)
> hist(BS.pop,breaks =20)
> hist(df$Waittime, breaks =20)
> single.xbar =  mean(single.samp)
Error in mean(single.samp) : object 'single.samp' not found
> single.xbar =  mean(sample.samp)
> BS.xbars = rep(0,10000)
> for (i in 1:10000){
+ sample.samp = sample(BS.pop,300, replace =T)
+ BS.xbars[i] = mean(sample.samp)
+ }
> hist(BS.xbars,breaks=20)
> lines(c(10,10),c(0,10000),lwd=2,col=2)
> lines(c(10.34,10.34),c(0,10000),lwd=2,col=2)
> pval = length(BS.xbars[BS.xbars>mean(df$Waittime)])/10000
> pval
[1] 0.2903
> # omg it's soo close to the theoretical method
> save()
Error in save() : 'file' must be specified
In addition: Warning message:
In save() : nothing specified to be save()d
> save('0125class.r')
Error in save("0125class.r") : object ‘0125class.r’ not found
> save('0125class.r')
Error in save("0125class.r") : object ‘0125class.r’ not found
> quit()

