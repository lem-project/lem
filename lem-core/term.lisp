(defpackage :lem.term
  (:use :cl)
  (:export :get-color-pair
           :term-set-foreground
           :term-set-background
           :background-mode
           :term-init
           :term-finalize
           :term-set-tty))
(in-package :lem.term)

(cffi:defcvar ("COLOR_PAIRS" *COLOR-PAIRS* :library charms/ll::libcurses) :int)


(defparameter *rgb.txt* "! $Xorg: rgb.txt,v 1.3 2000/08/17 19:54:00 cpqbld Exp $
255 250 250  snow
248 248 255  ghost white
248 248 255  GhostWhite
245 245 245  white smoke
245 245 245  WhiteSmoke
220 220 220  gainsboro
255 250 240  floral white
255 250 240  FloralWhite
253 245 230  old lace
253 245 230  OldLace
250 240 230  linen
250 235 215  antique white
250 235 215  AntiqueWhite
255 239 213  papaya whip
255 239 213  PapayaWhip
255 235 205  blanched almond
255 235 205  BlanchedAlmond
255 228 196  bisque
255 218 185  peach puff
255 218 185  PeachPuff
255 222 173  navajo white
255 222 173  NavajoWhite
255 228 181  moccasin
255 248 220  cornsilk
255 255 240  ivory
255 250 205  lemon chiffon
255 250 205  LemonChiffon
255 245 238  seashell
240 255 240  honeydew
245 255 250  mint cream
245 255 250  MintCream
240 255 255  azure
240 248 255  alice blue
240 248 255  AliceBlue
230 230 250  lavender
255 240 245  lavender blush
255 240 245  LavenderBlush
255 228 225  misty rose
255 228 225  MistyRose
255 255 255  white
  0   0   0  black
 47  79  79  dark slate gray
 47  79  79  DarkSlateGray
 47  79  79  dark slate grey
 47  79  79  DarkSlateGrey
105 105 105  dim gray
105 105 105  DimGray
105 105 105  dim grey
105 105 105  DimGrey
112 128 144  slate gray
112 128 144  SlateGray
112 128 144  slate grey
112 128 144  SlateGrey
119 136 153  light slate gray
119 136 153  LightSlateGray
119 136 153  light slate grey
119 136 153  LightSlateGrey
190 190 190  gray
190 190 190  grey
211 211 211  light grey
211 211 211  LightGrey
211 211 211  light gray
211 211 211  LightGray
 25  25 112  midnight blue
 25  25 112  MidnightBlue
  0   0 128  navy
  0   0 128  navy blue
  0   0 128  NavyBlue
100 149 237  cornflower blue
100 149 237  CornflowerBlue
 72  61 139  dark slate blue
 72  61 139  DarkSlateBlue
106  90 205  slate blue
106  90 205  SlateBlue
123 104 238  medium slate blue
123 104 238  MediumSlateBlue
132 112 255  light slate blue
132 112 255  LightSlateBlue
  0   0 205  medium blue
  0   0 205  MediumBlue
 65 105 225  royal blue
 65 105 225  RoyalBlue
  0   0 255  blue
 30 144 255  dodger blue
 30 144 255  DodgerBlue
  0 191 255  deep sky blue
  0 191 255  DeepSkyBlue
135 206 235  sky blue
135 206 235  SkyBlue
135 206 250  light sky blue
135 206 250  LightSkyBlue
 70 130 180  steel blue
 70 130 180  SteelBlue
176 196 222  light steel blue
176 196 222  LightSteelBlue
173 216 230  light blue
173 216 230  LightBlue
176 224 230  powder blue
176 224 230  PowderBlue
175 238 238  pale turquoise
175 238 238  PaleTurquoise
  0 206 209  dark turquoise
  0 206 209  DarkTurquoise
 72 209 204  medium turquoise
 72 209 204  MediumTurquoise
 64 224 208  turquoise
  0 255 255  cyan
224 255 255  light cyan
224 255 255  LightCyan
 95 158 160  cadet blue
 95 158 160  CadetBlue
102 205 170  medium aquamarine
102 205 170  MediumAquamarine
127 255 212  aquamarine
  0 100   0  dark green
  0 100   0  DarkGreen
 85 107  47  dark olive green
 85 107  47  DarkOliveGreen
143 188 143  dark sea green
143 188 143  DarkSeaGreen
 46 139  87  sea green
 46 139  87  SeaGreen
 60 179 113  medium sea green
 60 179 113  MediumSeaGreen
 32 178 170  light sea green
 32 178 170  LightSeaGreen
152 251 152  pale green
152 251 152  PaleGreen
  0 255 127  spring green
  0 255 127  SpringGreen
124 252   0  lawn green
124 252   0  LawnGreen
  0 255   0  green
127 255   0  chartreuse
  0 250 154  medium spring green
  0 250 154  MediumSpringGreen
173 255  47  green yellow
173 255  47  GreenYellow
 50 205  50  lime green
 50 205  50  LimeGreen
154 205  50  yellow green
154 205  50  YellowGreen
 34 139  34  forest green
 34 139  34  ForestGreen
107 142  35  olive drab
107 142  35  OliveDrab
189 183 107  dark khaki
189 183 107  DarkKhaki
240 230 140  khaki
238 232 170  pale goldenrod
238 232 170  PaleGoldenrod
250 250 210  light goldenrod yellow
250 250 210  LightGoldenrodYellow
255 255 224  light yellow
255 255 224  LightYellow
255 255   0  yellow
255 215   0   gold
238 221 130  light goldenrod
238 221 130  LightGoldenrod
218 165  32  goldenrod
184 134  11  dark goldenrod
184 134  11  DarkGoldenrod
188 143 143  rosy brown
188 143 143  RosyBrown
205  92  92  indian red
205  92  92  IndianRed
139  69  19  saddle brown
139  69  19  SaddleBrown
160  82  45  sienna
205 133  63  peru
222 184 135  burlywood
245 245 220  beige
245 222 179  wheat
244 164  96  sandy brown
244 164  96  SandyBrown
210 180 140  tan
210 105  30  chocolate
178  34  34  firebrick
165  42  42  brown
233 150 122  dark salmon
233 150 122  DarkSalmon
250 128 114  salmon
255 160 122  light salmon
255 160 122  LightSalmon
255 165   0  orange
255 140   0  dark orange
255 140   0  DarkOrange
255 127  80  coral
240 128 128  light coral
240 128 128  LightCoral
255  99  71  tomato
255  69   0  orange red
255  69   0  OrangeRed
255   0   0  red
255 105 180  hot pink
255 105 180  HotPink
255  20 147  deep pink
255  20 147  DeepPink
255 192 203  pink
255 182 193  light pink
255 182 193  LightPink
219 112 147  pale violet red
219 112 147  PaleVioletRed
176  48  96  maroon
199  21 133  medium violet red
199  21 133  MediumVioletRed
208  32 144  violet red
208  32 144  VioletRed
255   0 255  magenta
238 130 238  violet
221 160 221  plum
218 112 214  orchid
186  85 211  medium orchid
186  85 211  MediumOrchid
153  50 204  dark orchid
153  50 204  DarkOrchid
148   0 211  dark violet
148   0 211  DarkViolet
138  43 226  blue violet
138  43 226  BlueViolet
160  32 240  purple
147 112 219  medium purple
147 112 219  MediumPurple
216 191 216  thistle
255 250 250  snow1
238 233 233  snow2
205 201 201  snow3
139 137 137  snow4
255 245 238  seashell1
238 229 222  seashell2
205 197 191  seashell3
139 134 130  seashell4
255 239 219  AntiqueWhite1
238 223 204  AntiqueWhite2
205 192 176  AntiqueWhite3
139 131 120  AntiqueWhite4
255 228 196  bisque1
238 213 183  bisque2
205 183 158  bisque3
139 125 107  bisque4
255 218 185  PeachPuff1
238 203 173  PeachPuff2
205 175 149  PeachPuff3
139 119 101  PeachPuff4
255 222 173  NavajoWhite1
238 207 161  NavajoWhite2
205 179 139  NavajoWhite3
139 121  94  NavajoWhite4
255 250 205  LemonChiffon1
238 233 191  LemonChiffon2
205 201 165  LemonChiffon3
139 137 112  LemonChiffon4
255 248 220  cornsilk1
238 232 205  cornsilk2
205 200 177  cornsilk3
139 136 120  cornsilk4
255 255 240  ivory1
238 238 224  ivory2
205 205 193  ivory3
139 139 131  ivory4
240 255 240  honeydew1
224 238 224  honeydew2
193 205 193  honeydew3
131 139 131  honeydew4
255 240 245  LavenderBlush1
238 224 229  LavenderBlush2
205 193 197  LavenderBlush3
139 131 134  LavenderBlush4
255 228 225  MistyRose1
238 213 210  MistyRose2
205 183 181  MistyRose3
139 125 123  MistyRose4
240 255 255  azure1
224 238 238  azure2
193 205 205  azure3
131 139 139  azure4
131 111 255  SlateBlue1
122 103 238  SlateBlue2
105  89 205  SlateBlue3
 71  60 139  SlateBlue4
 72 118 255  RoyalBlue1
 67 110 238  RoyalBlue2
 58  95 205  RoyalBlue3
 39  64 139  RoyalBlue4
  0   0 255  blue1
  0   0 238  blue2
  0   0 205  blue3
  0   0 139  blue4
 30 144 255  DodgerBlue1
 28 134 238  DodgerBlue2
 24 116 205  DodgerBlue3
 16  78 139  DodgerBlue4
 99 184 255  SteelBlue1
 92 172 238  SteelBlue2
 79 148 205  SteelBlue3
 54 100 139  SteelBlue4
  0 191 255  DeepSkyBlue1
  0 178 238  DeepSkyBlue2
  0 154 205  DeepSkyBlue3
  0 104 139  DeepSkyBlue4
135 206 255  SkyBlue1
126 192 238  SkyBlue2
108 166 205  SkyBlue3
 74 112 139  SkyBlue4
176 226 255  LightSkyBlue1
164 211 238  LightSkyBlue2
141 182 205  LightSkyBlue3
 96 123 139  LightSkyBlue4
198 226 255  SlateGray1
185 211 238  SlateGray2
159 182 205  SlateGray3
108 123 139  SlateGray4
202 225 255  LightSteelBlue1
188 210 238  LightSteelBlue2
162 181 205  LightSteelBlue3
110 123 139  LightSteelBlue4
191 239 255  LightBlue1
178 223 238  LightBlue2
154 192 205  LightBlue3
104 131 139  LightBlue4
224 255 255  LightCyan1
209 238 238  LightCyan2
180 205 205  LightCyan3
122 139 139  LightCyan4
187 255 255  PaleTurquoise1
174 238 238  PaleTurquoise2
150 205 205  PaleTurquoise3
102 139 139  PaleTurquoise4
152 245 255  CadetBlue1
142 229 238  CadetBlue2
122 197 205  CadetBlue3
 83 134 139  CadetBlue4
  0 245 255  turquoise1
  0 229 238  turquoise2
  0 197 205  turquoise3
  0 134 139  turquoise4
  0 255 255  cyan1
  0 238 238  cyan2
  0 205 205  cyan3
  0 139 139  cyan4
151 255 255  DarkSlateGray1
141 238 238  DarkSlateGray2
121 205 205  DarkSlateGray3
 82 139 139  DarkSlateGray4
127 255 212  aquamarine1
118 238 198  aquamarine2
102 205 170  aquamarine3
 69 139 116  aquamarine4
193 255 193  DarkSeaGreen1
180 238 180  DarkSeaGreen2
155 205 155  DarkSeaGreen3
105 139 105  DarkSeaGreen4
 84 255 159  SeaGreen1
 78 238 148  SeaGreen2
 67 205 128  SeaGreen3
 46 139  87  SeaGreen4
154 255 154  PaleGreen1
144 238 144  PaleGreen2
124 205 124  PaleGreen3
 84 139  84  PaleGreen4
  0 255 127  SpringGreen1
  0 238 118  SpringGreen2
  0 205 102  SpringGreen3
  0 139  69  SpringGreen4
  0 255   0  green1
  0 238   0  green2
  0 205   0  green3
  0 139   0  green4
127 255   0  chartreuse1
118 238   0  chartreuse2
102 205   0  chartreuse3
 69 139   0  chartreuse4
192 255  62  OliveDrab1
179 238  58  OliveDrab2
154 205  50  OliveDrab3
105 139  34  OliveDrab4
202 255 112  DarkOliveGreen1
188 238 104  DarkOliveGreen2
162 205  90  DarkOliveGreen3
110 139  61  DarkOliveGreen4
255 246 143  khaki1
238 230 133  khaki2
205 198 115  khaki3
139 134  78  khaki4
255 236 139  LightGoldenrod1
238 220 130  LightGoldenrod2
205 190 112  LightGoldenrod3
139 129  76  LightGoldenrod4
255 255 224  LightYellow1
238 238 209  LightYellow2
205 205 180  LightYellow3
139 139 122  LightYellow4
255 255   0  yellow1
238 238   0  yellow2
205 205   0  yellow3
139 139   0  yellow4
255 215   0  gold1
238 201   0  gold2
205 173   0  gold3
139 117   0  gold4
255 193  37  goldenrod1
238 180  34  goldenrod2
205 155  29  goldenrod3
139 105  20  goldenrod4
255 185  15  DarkGoldenrod1
238 173  14  DarkGoldenrod2
205 149  12  DarkGoldenrod3
139 101   8  DarkGoldenrod4
255 193 193  RosyBrown1
238 180 180  RosyBrown2
205 155 155  RosyBrown3
139 105 105  RosyBrown4
255 106 106  IndianRed1
238  99  99  IndianRed2
205  85  85  IndianRed3
139  58  58  IndianRed4
255 130  71  sienna1
238 121  66  sienna2
205 104  57  sienna3
139  71  38  sienna4
255 211 155  burlywood1
238 197 145  burlywood2
205 170 125  burlywood3
139 115  85  burlywood4
255 231 186  wheat1
238 216 174  wheat2
205 186 150  wheat3
139 126 102  wheat4
255 165  79  tan1
238 154  73  tan2
205 133  63  tan3
139  90  43  tan4
255 127  36  chocolate1
238 118  33  chocolate2
205 102  29  chocolate3
139  69  19  chocolate4
255  48  48  firebrick1
238  44  44  firebrick2
205  38  38  firebrick3
139  26  26  firebrick4
255  64  64  brown1
238  59  59  brown2
205  51  51  brown3
139  35  35  brown4
255 140 105  salmon1
238 130  98  salmon2
205 112  84  salmon3
139  76  57  salmon4
255 160 122  LightSalmon1
238 149 114  LightSalmon2
205 129  98  LightSalmon3
139  87  66  LightSalmon4
255 165   0  orange1
238 154   0  orange2
205 133   0  orange3
139  90   0  orange4
255 127   0  DarkOrange1
238 118   0  DarkOrange2
205 102   0  DarkOrange3
139  69   0  DarkOrange4
255 114  86  coral1
238 106  80  coral2
205  91  69  coral3
139  62  47  coral4
255  99  71  tomato1
238  92  66  tomato2
205  79  57  tomato3
139  54  38  tomato4
255  69   0  OrangeRed1
238  64   0  OrangeRed2
205  55   0  OrangeRed3
139  37   0  OrangeRed4
255   0   0  red1
238   0   0  red2
205   0   0  red3
139   0   0  red4
215   7  81  DebianRed
255  20 147  DeepPink1
238  18 137  DeepPink2
205  16 118  DeepPink3
139  10  80  DeepPink4
255 110 180  HotPink1
238 106 167  HotPink2
205  96 144  HotPink3
139  58  98  HotPink4
255 181 197  pink1
238 169 184  pink2
205 145 158  pink3
139  99 108  pink4
255 174 185  LightPink1
238 162 173  LightPink2
205 140 149  LightPink3
139  95 101  LightPink4
255 130 171  PaleVioletRed1
238 121 159  PaleVioletRed2
205 104 137  PaleVioletRed3
139  71  93  PaleVioletRed4
255  52 179  maroon1
238  48 167  maroon2
205  41 144  maroon3
139  28  98  maroon4
255  62 150  VioletRed1
238  58 140  VioletRed2
205  50 120  VioletRed3
139  34  82  VioletRed4
255   0 255  magenta1
238   0 238  magenta2
205   0 205  magenta3
139   0 139  magenta4
255 131 250  orchid1
238 122 233  orchid2
205 105 201  orchid3
139  71 137  orchid4
255 187 255  plum1
238 174 238  plum2
205 150 205  plum3
139 102 139  plum4
224 102 255  MediumOrchid1
209  95 238  MediumOrchid2
180  82 205  MediumOrchid3
122  55 139  MediumOrchid4
191  62 255  DarkOrchid1
178  58 238  DarkOrchid2
154  50 205  DarkOrchid3
104  34 139  DarkOrchid4
155  48 255  purple1
145  44 238  purple2
125  38 205  purple3
 85  26 139  purple4
171 130 255  MediumPurple1
159 121 238  MediumPurple2
137 104 205  MediumPurple3
 93  71 139  MediumPurple4
255 225 255  thistle1
238 210 238  thistle2
205 181 205  thistle3
139 123 139  thistle4
  0   0   0  gray0
  0   0   0  grey0
  3   3   3  gray1
  3   3   3  grey1
  5   5   5  gray2
  5   5   5  grey2
  8   8   8  gray3
  8   8   8  grey3
 10  10  10   gray4
 10  10  10   grey4
 13  13  13   gray5
 13  13  13   grey5
 15  15  15   gray6
 15  15  15   grey6
 18  18  18   gray7
 18  18  18   grey7
 20  20  20   gray8
 20  20  20   grey8
 23  23  23   gray9
 23  23  23   grey9
 26  26  26   gray10
 26  26  26   grey10
 28  28  28   gray11
 28  28  28   grey11
 31  31  31   gray12
 31  31  31   grey12
 33  33  33   gray13
 33  33  33   grey13
 36  36  36   gray14
 36  36  36   grey14
 38  38  38   gray15
 38  38  38   grey15
 41  41  41   gray16
 41  41  41   grey16
 43  43  43   gray17
 43  43  43   grey17
 46  46  46   gray18
 46  46  46   grey18
 48  48  48   gray19
 48  48  48   grey19
 51  51  51   gray20
 51  51  51   grey20
 54  54  54   gray21
 54  54  54   grey21
 56  56  56   gray22
 56  56  56   grey22
 59  59  59   gray23
 59  59  59   grey23
 61  61  61   gray24
 61  61  61   grey24
 64  64  64   gray25
 64  64  64   grey25
 66  66  66   gray26
 66  66  66   grey26
 69  69  69   gray27
 69  69  69   grey27
 71  71  71   gray28
 71  71  71   grey28
 74  74  74   gray29
 74  74  74   grey29
 77  77  77   gray30
 77  77  77   grey30
 79  79  79   gray31
 79  79  79   grey31
 82  82  82   gray32
 82  82  82   grey32
 84  84  84   gray33
 84  84  84   grey33
 87  87  87   gray34
 87  87  87   grey34
 89  89  89   gray35
 89  89  89   grey35
 92  92  92   gray36
 92  92  92   grey36
 94  94  94   gray37
 94  94  94   grey37
 97  97  97   gray38
 97  97  97   grey38
 99  99  99   gray39
 99  99  99   grey39
102 102 102   gray40
102 102 102   grey40
105 105 105   gray41
105 105 105   grey41
107 107 107   gray42
107 107 107   grey42
110 110 110   gray43
110 110 110   grey43
112 112 112   gray44
112 112 112   grey44
115 115 115   gray45
115 115 115   grey45
117 117 117   gray46
117 117 117   grey46
120 120 120   gray47
120 120 120   grey47
122 122 122   gray48
122 122 122   grey48
125 125 125   gray49
125 125 125   grey49
127 127 127   gray50
127 127 127   grey50
130 130 130   gray51
130 130 130   grey51
133 133 133   gray52
133 133 133   grey52
135 135 135   gray53
135 135 135   grey53
138 138 138   gray54
138 138 138   grey54
140 140 140   gray55
140 140 140   grey55
143 143 143   gray56
143 143 143   grey56
145 145 145   gray57
145 145 145   grey57
148 148 148   gray58
148 148 148   grey58
150 150 150   gray59
150 150 150   grey59
153 153 153   gray60
153 153 153   grey60
156 156 156   gray61
156 156 156   grey61
158 158 158   gray62
158 158 158   grey62
161 161 161   gray63
161 161 161   grey63
163 163 163   gray64
163 163 163   grey64
166 166 166   gray65
166 166 166   grey65
168 168 168   gray66
168 168 168   grey66
171 171 171   gray67
171 171 171   grey67
173 173 173   gray68
173 173 173   grey68
176 176 176   gray69
176 176 176   grey69
179 179 179   gray70
179 179 179   grey70
181 181 181   gray71
181 181 181   grey71
184 184 184   gray72
184 184 184   grey72
186 186 186   gray73
186 186 186   grey73
189 189 189   gray74
189 189 189   grey74
191 191 191   gray75
191 191 191   grey75
194 194 194   gray76
194 194 194   grey76
196 196 196   gray77
196 196 196   grey77
199 199 199   gray78
199 199 199   grey78
201 201 201   gray79
201 201 201   grey79
204 204 204   gray80
204 204 204   grey80
207 207 207   gray81
207 207 207   grey81
209 209 209   gray82
209 209 209   grey82
212 212 212   gray83
212 212 212   grey83
214 214 214   gray84
214 214 214   grey84
217 217 217   gray85
217 217 217   grey85
219 219 219   gray86
219 219 219   grey86
222 222 222   gray87
222 222 222   grey87
224 224 224   gray88
224 224 224   grey88
227 227 227   gray89
227 227 227   grey89
229 229 229   gray90
229 229 229   grey90
232 232 232   gray91
232 232 232   grey91
235 235 235   gray92
235 235 235   grey92
237 237 237   gray93
237 237 237   grey93
240 240 240   gray94
240 240 240   grey94
242 242 242   gray95
242 242 242   grey95
245 245 245   gray96
245 245 245   grey96
247 247 247   gray97
247 247 247   grey97
250 250 250   gray98
250 250 250   grey98
252 252 252   gray99
252 252 252   grey99
255 255 255   gray100
255 255 255   grey100
169 169 169  dark grey
169 169 169  DarkGrey
169 169 169  dark gray
169 169 169  DarkGray
0     0 139  dark blue
0     0 139  DarkBlue
0   139 139  dark cyan
0   139 139  DarkCyan
139   0 139  dark magenta
139   0 139  DarkMagenta
139   0   0  dark red
139   0   0  DarkRed
144 238 144  light green
144 238 144  LightGreen
")

(defparameter *color-names*
  (flet ((parse (text)
           (with-input-from-string (stream text)
             (loop :for line := (read-line stream nil)
                   :while line
                   :for elt := (ppcre:register-groups-bind (r g b name)
                                   ("^\\s*(\\d+)\\s+(\\d+)\\s+(\\d+)\\s+([a-zA-Z0-9 ]+)" line)
                                 (cons name
                                       (list (and r (parse-integer r))
                                             (and g (parse-integer g))
                                             (and b (parse-integer b)))))
                   :if elt
                   :collect elt))))
    (alexandria:alist-hash-table (parse *rgb.txt*) :test 'equal)))

(defvar *colors*)

(defun color-red (color) (first color))
(defun color-green (color) (second color))
(defun color-blue (color) (third color))
(defun color-number (color) (fourth color))

(defun init-colors (n)
  (let ((counter 0))
    (flet ((add-color (r g b)
             (charms/ll:init-color counter
                                   (round (* r 1000/255))
                                   (round (* g 1000/255))
                                   (round (* b 1000/255)))
             (setf (aref *colors* counter) (list r g b counter))
             (incf counter)))
      (setf *colors* (make-array n))
      (add-color #x00 #x00 #x00)
      (add-color #xcd #x00 #x00)
      (add-color #x00 #xcd #x00)
      (add-color #xcd #xcd #x00)
      (add-color #x00 #x00 #xee)
      (add-color #xcd #x00 #xcd)
      (add-color #x00 #xcd #xcd)
      (add-color #xe5 #xe5 #xe5)
      (when (<= 16 n)
        (add-color #x7f #x7f #x7f)
        (add-color #xff #x00 #x00)
        (add-color #x00 #xff #x00)
        (add-color #xff #xff #x00)
        (add-color #x5c #x5c #xff)
        (add-color #xff #x00 #xff)
        (add-color #x00 #xff #xff)
        (add-color #xff #xff #xff))
      (when (<= 256 n)
        (add-color #x00 #x00 #x00)
        (add-color #x00 #x00 #x5f)
        (add-color #x00 #x00 #x87)
        (add-color #x00 #x00 #xaf)
        (add-color #x00 #x00 #xd7)
        (add-color #x00 #x00 #xff)
        (add-color #x00 #x5f #x00)
        (add-color #x00 #x5f #x5f)
        (add-color #x00 #x5f #x87)
        (add-color #x00 #x5f #xaf)
        (add-color #x00 #x5f #xd7)
        (add-color #x00 #x5f #xff)
        (add-color #x00 #x87 #x00)
        (add-color #x00 #x87 #x5f)
        (add-color #x00 #x87 #x87)
        (add-color #x00 #x87 #xaf)
        (add-color #x00 #x87 #xd7)
        (add-color #x00 #x87 #xff)
        (add-color #x00 #xaf #x00)
        (add-color #x00 #xaf #x5f)
        (add-color #x00 #xaf #x87)
        (add-color #x00 #xaf #xaf)
        (add-color #x00 #xaf #xd7)
        (add-color #x00 #xaf #xff)
        (add-color #x00 #xd7 #x00)
        (add-color #x00 #xd7 #x5f)
        (add-color #x00 #xd7 #x87)
        (add-color #x00 #xd7 #xaf)
        (add-color #x00 #xd7 #xd7)
        (add-color #x00 #xd7 #xff)
        (add-color #x00 #xff #x00)
        (add-color #x00 #xff #x5f)
        (add-color #x00 #xff #x87)
        (add-color #x00 #xff #xaf)
        (add-color #x00 #xff #xd7)
        (add-color #x00 #xff #xff)
        (add-color #x5f #x00 #x00)
        (add-color #x5f #x00 #x5f)
        (add-color #x5f #x00 #x87)
        (add-color #x5f #x00 #xaf)
        (add-color #x5f #x00 #xd7)
        (add-color #x5f #x00 #xff)
        (add-color #x5f #x5f #x00)
        (add-color #x5f #x5f #x5f)
        (add-color #x5f #x5f #x87)
        (add-color #x5f #x5f #xaf)
        (add-color #x5f #x5f #xd7)
        (add-color #x5f #x5f #xff)
        (add-color #x5f #x87 #x00)
        (add-color #x5f #x87 #x5f)
        (add-color #x5f #x87 #x87)
        (add-color #x5f #x87 #xaf)
        (add-color #x5f #x87 #xd7)
        (add-color #x5f #x87 #xff)
        (add-color #x5f #xaf #x00)
        (add-color #x5f #xaf #x5f)
        (add-color #x5f #xaf #x87)
        (add-color #x5f #xaf #xaf)
        (add-color #x5f #xaf #xd7)
        (add-color #x5f #xaf #xff)
        (add-color #x5f #xd7 #x00)
        (add-color #x5f #xd7 #x5f)
        (add-color #x5f #xd7 #x87)
        (add-color #x5f #xd7 #xaf)
        (add-color #x5f #xd7 #xd7)
        (add-color #x5f #xd7 #xff)
        (add-color #x5f #xff #x00)
        (add-color #x5f #xff #x5f)
        (add-color #x5f #xff #x87)
        (add-color #x5f #xff #xaf)
        (add-color #x5f #xff #xd7)
        (add-color #x5f #xff #xff)
        (add-color #x87 #x00 #x00)
        (add-color #x87 #x00 #x5f)
        (add-color #x87 #x00 #x87)
        (add-color #x87 #x00 #xaf)
        (add-color #x87 #x00 #xd7)
        (add-color #x87 #x00 #xff)
        (add-color #x87 #x5f #x00)
        (add-color #x87 #x5f #x5f)
        (add-color #x87 #x5f #x87)
        (add-color #x87 #x5f #xaf)
        (add-color #x87 #x5f #xd7)
        (add-color #x87 #x5f #xff)
        (add-color #x87 #x87 #x00)
        (add-color #x87 #x87 #x5f)
        (add-color #x87 #x87 #x87)
        (add-color #x87 #x87 #xaf)
        (add-color #x87 #x87 #xd7)
        (add-color #x87 #x87 #xff)
        (add-color #x87 #xaf #x00)
        (add-color #x87 #xaf #x5f)
        (add-color #x87 #xaf #x87)
        (add-color #x87 #xaf #xaf)
        (add-color #x87 #xaf #xd7)
        (add-color #x87 #xaf #xff)
        (add-color #x87 #xd7 #x00)
        (add-color #x87 #xd7 #x5f)
        (add-color #x87 #xd7 #x87)
        (add-color #x87 #xd7 #xaf)
        (add-color #x87 #xd7 #xd7)
        (add-color #x87 #xd7 #xff)
        (add-color #x87 #xff #x00)
        (add-color #x87 #xff #x5f)
        (add-color #x87 #xff #x87)
        (add-color #x87 #xff #xaf)
        (add-color #x87 #xff #xd7)
        (add-color #x87 #xff #xff)
        (add-color #xaf #x00 #x00)
        (add-color #xaf #x00 #x5f)
        (add-color #xaf #x00 #x87)
        (add-color #xaf #x00 #xaf)
        (add-color #xaf #x00 #xd7)
        (add-color #xaf #x00 #xff)
        (add-color #xaf #x5f #x00)
        (add-color #xaf #x5f #x5f)
        (add-color #xaf #x5f #x87)
        (add-color #xaf #x5f #xaf)
        (add-color #xaf #x5f #xd7)
        (add-color #xaf #x5f #xff)
        (add-color #xaf #x87 #x00)
        (add-color #xaf #x87 #x5f)
        (add-color #xaf #x87 #x87)
        (add-color #xaf #x87 #xaf)
        (add-color #xaf #x87 #xd7)
        (add-color #xaf #x87 #xff)
        (add-color #xaf #xaf #x00)
        (add-color #xaf #xaf #x5f)
        (add-color #xaf #xaf #x87)
        (add-color #xaf #xaf #xaf)
        (add-color #xaf #xaf #xd7)
        (add-color #xaf #xaf #xff)
        (add-color #xaf #xd7 #x00)
        (add-color #xaf #xd7 #x5f)
        (add-color #xaf #xd7 #x87)
        (add-color #xaf #xd7 #xaf)
        (add-color #xaf #xd7 #xd7)
        (add-color #xaf #xd7 #xff)
        (add-color #xaf #xff #x00)
        (add-color #xaf #xff #x5f)
        (add-color #xaf #xff #x87)
        (add-color #xaf #xff #xaf)
        (add-color #xaf #xff #xd7)
        (add-color #xaf #xff #xff)
        (add-color #xd7 #x00 #x00)
        (add-color #xd7 #x00 #x5f)
        (add-color #xd7 #x00 #x87)
        (add-color #xd7 #x00 #xaf)
        (add-color #xd7 #x00 #xd7)
        (add-color #xd7 #x00 #xff)
        (add-color #xd7 #x5f #x00)
        (add-color #xd7 #x5f #x5f)
        (add-color #xd7 #x5f #x87)
        (add-color #xd7 #x5f #xaf)
        (add-color #xd7 #x5f #xd7)
        (add-color #xd7 #x5f #xff)
        (add-color #xd7 #x87 #x00)
        (add-color #xd7 #x87 #x5f)
        (add-color #xd7 #x87 #x87)
        (add-color #xd7 #x87 #xaf)
        (add-color #xd7 #x87 #xd7)
        (add-color #xd7 #x87 #xff)
        (add-color #xd7 #xaf #x00)
        (add-color #xd7 #xaf #x5f)
        (add-color #xd7 #xaf #x87)
        (add-color #xd7 #xaf #xaf)
        (add-color #xd7 #xaf #xd7)
        (add-color #xd7 #xaf #xff)
        (add-color #xd7 #xd7 #x00)
        (add-color #xd7 #xd7 #x5f)
        (add-color #xd7 #xd7 #x87)
        (add-color #xd7 #xd7 #xaf)
        (add-color #xd7 #xd7 #xd7)
        (add-color #xd7 #xd7 #xff)
        (add-color #xd7 #xff #x00)
        (add-color #xd7 #xff #x5f)
        (add-color #xd7 #xff #x87)
        (add-color #xd7 #xff #xaf)
        (add-color #xd7 #xff #xd7)
        (add-color #xd7 #xff #xff)
        (add-color #xff #x00 #x00)
        (add-color #xff #x00 #x5f)
        (add-color #xff #x00 #x87)
        (add-color #xff #x00 #xaf)
        (add-color #xff #x00 #xd7)
        (add-color #xff #x00 #xff)
        (add-color #xff #x5f #x00)
        (add-color #xff #x5f #x5f)
        (add-color #xff #x5f #x87)
        (add-color #xff #x5f #xaf)
        (add-color #xff #x5f #xd7)
        (add-color #xff #x5f #xff)
        (add-color #xff #x87 #x00)
        (add-color #xff #x87 #x5f)
        (add-color #xff #x87 #x87)
        (add-color #xff #x87 #xaf)
        (add-color #xff #x87 #xd7)
        (add-color #xff #x87 #xff)
        (add-color #xff #xaf #x00)
        (add-color #xff #xaf #x5f)
        (add-color #xff #xaf #x87)
        (add-color #xff #xaf #xaf)
        (add-color #xff #xaf #xd7)
        (add-color #xff #xaf #xff)
        (add-color #xff #xd7 #x00)
        (add-color #xff #xd7 #x5f)
        (add-color #xff #xd7 #x87)
        (add-color #xff #xd7 #xaf)
        (add-color #xff #xd7 #xd7)
        (add-color #xff #xd7 #xff)
        (add-color #xff #xff #x00)
        (add-color #xff #xff #x5f)
        (add-color #xff #xff #x87)
        (add-color #xff #xff #xaf)
        (add-color #xff #xff #xd7)
        (add-color #xff #xff #xff)
        (add-color #x08 #x08 #x08)
        (add-color #x12 #x12 #x12)
        (add-color #x1c #x1c #x1c)
        (add-color #x26 #x26 #x26)
        (add-color #x30 #x30 #x30)
        (add-color #x3a #x3a #x3a)
        (add-color #x44 #x44 #x44)
        (add-color #x4e #x4e #x4e)
        (add-color #x58 #x58 #x58)
        (add-color #x62 #x62 #x62)
        (add-color #x6c #x6c #x6c)
        (add-color #x76 #x76 #x76)
        (add-color #x80 #x80 #x80)
        (add-color #x8a #x8a #x8a)
        (add-color #x94 #x94 #x94)
        (add-color #x9e #x9e #x9e)
        (add-color #xa8 #xa8 #xa8)
        (add-color #xb2 #xb2 #xb2)
        (add-color #xbc #xbc #xbc)
        (add-color #xc6 #xc6 #xc6)
        (add-color #xd0 #xd0 #xd0)
        (add-color #xda #xda #xda)
        (add-color #xe4 #xe4 #xe4)
        (add-color #xee #xee #xee)))))

(defun rgb-to-hsv (r g b)
  (let ((max (max r g b))
        (min (min r g b)))
    (let ((h (cond ((= min max) 0)
                   ((= r max)
                    (* 60 (/ (- g b) (- max min))))
                   ((= g max)
                    (+ 120 (* 60 (/ (- b r) (- max min)))))
                   ((= b max)
                    (+ 240 (* 60 (/ (- r g) (- max min))))))))
      (when (minusp h) (incf h 360))
      (let ((s (if (= min max) 0 (* 100 (/ (- max min) max))))
            (v (* 100 (/ max 255))))
        (values (round (float h))
                (round (float s))
                (round (float v)))))))

(defun rgb-to-hsv-distance (r1 g1 b1 r2 g2 b2)
  (multiple-value-bind (h1 s1 v1) (rgb-to-hsv r1 g1 b1)
    (multiple-value-bind (h2 s2 v2) (rgb-to-hsv r2 g2 b2)
      (let ((h (abs (- h1 h2)))
            (s (abs (- s1 s2)))
            (v (abs (- v1 v2))))
        (+ (* h h) (* s s) (* v v))))))

(defun get-color-rgb (r g b)
  (let ((min most-positive-fixnum)
        (best-color))
    (loop :for color :across *colors*
          :do (let ((dist (rgb-to-hsv-distance
                           r g b
                           (color-red color) (color-green color) (color-blue color))))
                (when (< dist min)
                  (setf min dist)
                  (setf best-color color))))
    (color-number best-color)))

(defun get-color-1 (string)
  (ppcre:register-groups-bind (r g b)
      ("^#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})$"
       string)
    (return-from get-color-1
      (get-color-rgb (and r (parse-integer r :radix 16))
                     (and g (parse-integer g :radix 16))
                     (and b (parse-integer b :radix 16)))))
  (ppcre:register-groups-bind (r g b)
      ("^#([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])$"
       string)
    (return-from get-color-1
      (get-color-rgb (and r (* 17 (parse-integer r :radix 16)))
                     (and g (* 17 (parse-integer g :radix 16)))
                     (and b (* 17 (parse-integer b :radix 16))))))
  (alexandria:when-let ((color (gethash string *color-names*)))
    (return-from get-color-1
      (get-color-rgb (color-red color) (color-green color) (color-blue color)))))

(defun get-color (string)
  (let ((color (get-color-1 string)))
    (if color
        (values color t)
        (values 0 nil))))


(defvar *pair-counter* 0)
(defvar *color-pair-table* (make-hash-table :test 'equal))

(defun init-pair (pair-color)
  (incf *pair-counter*)
  (charms/ll:init-pair *pair-counter* (car pair-color) (cdr pair-color))
  (setf (gethash pair-color *color-pair-table*)
        (charms/ll:color-pair *pair-counter*)))

(defun get-color-pair (fg-color-name bg-color-name)
  (let* ((fg-color (if (null fg-color-name) -1 (get-color fg-color-name)))
         (bg-color (if (null bg-color-name) -1 (get-color bg-color-name)))
         (pair-color (cons fg-color bg-color)))
    (cond ((gethash pair-color *color-pair-table*))
          ((< *pair-counter* *color-pairs*)
           (init-pair pair-color))
          (t 0))))

#+(or)
(defun get-color-content (n)
  (cffi:with-foreign-pointer (r (cffi:foreign-type-size '(:pointer :short)))
    (cffi:with-foreign-pointer (g (cffi:foreign-type-size '(:pointer :short)))
      (cffi:with-foreign-pointer (b (cffi:foreign-type-size '(:pointer :short)))
        (charms/ll:color-content n r g b)
        (list (cffi:mem-ref r :short)
              (cffi:mem-ref g :short)
              (cffi:mem-ref b :short))))))

(defun get-default-colors ()
  (cffi:with-foreign-pointer (f (cffi:foreign-type-size '(:pointer :short)))
    (cffi:with-foreign-pointer (b (cffi:foreign-type-size '(:pointer :short)))
      (charms/ll:pair-content 0 f b)
      (values (cffi:mem-ref f :short)
              (cffi:mem-ref b :short)))))

(defun set-default-color (foreground background)
  (let ((fg-color (if foreground (get-color foreground) -1))
        (bg-color (if background (get-color background) -1)))
    (charms/ll:assume-default-colors fg-color
                                     bg-color)))

(defun term-set-foreground (name)
  (multiple-value-bind (fg found) (get-color name)
    (let ((bg (nth-value 1 (get-default-colors))))
      (cond (found
             (charms/ll:assume-default-colors fg bg)
             t)
            (t
             (error "Undefined color: ~A" name))))))

(defun term-set-background (name)
  (multiple-value-bind (bg found) (get-color name)
    (let ((fg (nth-value 0 (get-default-colors))))
      (cond (found
             (charms/ll:assume-default-colors fg bg)
             t)
            (t
             (error "Undefined color: ~A" name))))))

(defun background-mode ()
  (let ((b (nth-value 1 (get-default-colors))))
    (cond ((= b -1) :light)
          (t
           (let ((color (aref *colors* b)))
             (let ((r (color-red color))
                   (g (color-green color))
                   (b (color-blue color)))
               (if (< 50 (/ (max r g b) 2.55))
                   :light
                   :dark)))))))

;;;

(cffi:defcfun "fopen" :pointer (path :string) (mode :string))
(cffi:defcfun "fclose" :int (fp :pointer))
(cffi:defcfun "fileno" :int (fd :pointer))

(cffi:defcstruct winsize
  (ws-row :unsigned-short)
  (ws-col :unsigned-short)
  (ws-xpixel :unsigned-short)
  (ws-ypixel :unsigned-short))

(cffi:defcfun ioctl :int
  (fd :int)
  (cmd :int)
  &rest)

(defvar *tty-name* nil)
(defvar *term-io* nil)

(defun resize-term ()
  (when *term-io*
    (cffi:with-foreign-object (ws '(:struct winsize))
      (when (= 0 (ioctl (fileno *term-io*) 21523 :pointer ws))
        (cffi:with-foreign-slots ((ws-row ws-col) ws (:struct winsize))
          (charms/ll:resizeterm ws-row ws-col))))))

(defun term-init-tty (tty-name)
  (let* ((io (fopen tty-name "r+")))
    (setf *term-io* io)
    (cffi:with-foreign-string (term "xterm")
      (charms/ll:newterm term io io))))

(defun term-init ()
  (if *tty-name*
      (term-init-tty *tty-name*)
      (charms/ll:initscr))
  (when (/= 0 (charms/ll:has-colors))
    (charms/ll:start-color)
    (init-colors charms/ll:*colors*)
    (set-default-color nil nil))
  (charms/ll:noecho)
  (charms/ll:cbreak)
  (charms/ll:raw)
  (charms/ll:nonl)
  (charms/ll:refresh)
  (charms/ll:keypad charms/ll:*stdscr* 1)
  ;(charms/ll:curs-set 0)
  )

(defun term-set-tty (tty-name)
  (setf *tty-name* tty-name))

(defun term-finalize ()
  (when *term-io*
    (fclose *term-io*)
    (setf *term-io* nil))
  (charms/ll:endwin)
  (charms/ll:delscreen charms/ll:*stdscr*))
