extensions [profiler NW]
globals [
  Rain               ;can only take values r_bad and r_good
  Ave-links          ;
  Ave-Reputation     ;
  R_initial          ;rain time step before to define auto correlation
  L                  ;total proportion of labor =1
  N_Contracts        ;number of fulfilled contracts per year
  N_DD               ;;number of intances with decition DD
  N_CD               ;;number of intances with decition CD
  N_DC               ;;number of intances with decition DC
  p_rain             ;information farmers have about the probability of next rainfall (good or bad year)
  T_contracts        ;;total number of fulfilled agreements
  T_CD               ;;total number of defections

  T_labor_off_farm
  c_p                ;;prospect theory parameter
  vf_G_DD            ;;value function wet year DD
  vf_B_DD            ;;value function dry year DD
  vf_G_CC            ;;value function wet year CC
  vf_B_CC            ;;value function dry year CC
  vf_Cc
  vf_Cd
  vf_Dc
  vf_Dd
  counts
Ccoef ;clustering coeficient
]

;###############################################################################################################
breed[Comunidad family]
undirected-link-breed [contracts contract]
directed-link-breed [friends friend]
undirected-link-breed [Ncontracts Ncontract]
;###############################################################################################################

;###############################################################################################################
Comunidad-own[
  Wealth                      ;Accumulated gain from interacting with other families and the environment
  Reputation                  ;Mean trust families that consider me a friend
  Agreement                   ;decition made by family before rainfall with a friend
  Final_action                ;final decision made individually after rainfall event
  productivity                ;yield per unit of labor
  current-partner-contract    ;Family with currently in contract setting
  score                       ;Score the partner after the rainfall Score = 3 for [CA CF]; score= 2 [DA, CF]; score= 1 [DA DF]; score = 0 [CA DF]  follow by
  trust_partner
  a_g
  a_l
  ref_point
  past_wealth
]

;###############################################################################################################
friends-own[
  p                           ;;probablity of maintaining the friendship link
  trust                       ;trust end1 has on end2 of link friend(end1 end2)
]


;###############################################################################################################
contracts-own[
]

Ncontracts-own[

  N-time-parners              ;number of time families linked by a friendship engaged in labor-sharing
]
;###############################################################################################################
to setup
  clear-all                    ;;clear all
  clear-links                  ;;clear links

  set R_initial 1
  ifelse enviromental-information = "yes"[set p_rain p_good][set p_rain 0.5]    ;;yes if farmers know the probability of a good year
  set N_Contracts 0
  set N_CD 0
                                                         ;;initial contracts=0
  set L 1
  set c_p 0.6                                                                      ;;total labor
  create-community                                                              ;;to create the farmers
  layout
  reset-ticks
end

;###############################################################################################################
to create-community
  create-Comunidad 40 [                                                                       ;create families
    setxy random-ycor random-xcor                                                             ;located them any place in the landscape
    set productivity (1 + random-normal 1 variability-Land-productivity)                      ;set productivity of the land
    ifelse (productivity < 0.1)[set productivity 0.1][set productivity productivity]          ;set productivity to be > 0.1
    set current-partner-contract "N"                                                          ;family with which they hace an agreement
    set agreement "N"                                                                         ;decision before rainfall
    set reputation 1
    if type_of_farmers ="risk-seeking" [
      set a_g 1.5
      set a_l 0.5
    ]
    if type_of_farmers ="risk-averse" [
      set a_g 0.5
      set a_l 1.5
    ]
    set ref_point 0
    set past_wealth [0 0 0 0 0]
    set shape "wheel"
    set size 1
    set color magenta
  ]
  ask Comunidad [
    create-friends-to other Comunidad                                          ; initial condition all families are friend to each other (fully connected)
    [
      set trust 1
      set p 1
      set color blue
      set hidden? true
    ]

  ]
end

;###############################################################################################################
to make-rain
  ifelse random-float 1 > temp_corr[
    let R_max ifelse-value ((1 + var_R) < 2) [1 + var_R][2]
    let R_min ifelse-value ((1 - var_R) > 0) [1 - var_R][0]
    set Rain ifelse-value (p_good > random-float 1)[R_max][R_min]
  ]
  [
    set Rain R_initial
  ]
  set R_initial Rain
end

;###############################################################################################################
to make-agreements

  ask Comunidad [
    if current-partner-contract = "N"[
      let cc max-one-of my-out-friends with [[current-partner-contract] of end2 = "N"] [p]
      if cc != nobody  [
        create-contract-with [end2] of cc[set hidden? true]
        set current-partner-contract [end2] of cc
        set agreement "C"
        ask [end2] of cc [
          set current-partner-contract myself
          set agreement "C"
        ]
      ]
    ]
  ]


  repeat 2 [
    ask Comunidad [
      if current-partner-contract = "N"[
        let cc max-one-of my-out-friends with [[current-partner-contract] of end2 = "N"] [trust]

        if cc != nobody  [
          create-contract-with [end2] of cc[set hidden? true]
          set current-partner-contract [end2] of cc
          set agreement "C"
          ask [end2] of cc [
            set current-partner-contract myself
            set agreement "C"
          ]
        ]
      ]
    ]
  ]


  ask contracts[
    let l1 end1
    let l2 end2
    ifelse (any? friends with [end1 = l1 and end2 = l2] and any? friends with [end1 = l2 and end2 = l1])[

      let f_a friends with [end1 = l1 and end2 = l2]
      let f_b friends with [end1 = l2 and end2 = l1]
      let p_a first [p] of f_a
      let p_b first [p] of f_b

      let ER_good (1 + var_R)
      let ER_bad (1 - var_R)

      let U_CC_GY ER_good * factor-of-cooperation *  ([productivity] of end1 * L + [productivity] of end2 * L)
      let U_CC_BY ER_bad * factor-of-cooperation *  ([productivity] of end1 * L + [productivity] of end2 * L)

      let U_DD_GY ER_good  *  ([productivity] of end1 * (L - Lw) + [productivity] of end2 * (L - Lw)) + 2 * Lw * wages
      let U_DD_BY ER_bad *  ([productivity] of end1 * (L - Lw) + [productivity] of end2 * (L - Lw)) + 2 * Lw * wages


      if decition-theory = "EU" [

        let E_U_CC p_rain * U_CC_GY +   (1 - p_rain) * U_CC_BY
        let E_U_DD p_rain * U_DD_GY +   (1 - p_rain) * U_DD_BY


        ifelse E_U_CC >= E_U_DD[
          ask end1 [
            set agreement "C"
            set trust_partner p_a
          ]
          ask end2 [
            set agreement "C"
            set trust_partner p_b
          ]
        ]
        [
          ask both-ends [
            set agreement "D"
            set score 1 / 3
          ]

        ]
      ]

      if decition-theory = "prospect-theory" [

        let delta_KG_DD   U_DD_GY - mean [ref_point] of both-ends
        let delta_KB_DD   U_DD_BY - mean [ref_point] of both-ends

        let delta_KG_CC   U_CC_GY - mean [ref_point] of both-ends
        let delta_KB_CC   U_CC_BY - mean [ref_point] of both-ends

        ifelse delta_KG_DD > 0 [set vf_G_DD ((abs delta_KG_DD) ^ (mean [a_g] of both-ends)) ][set vf_G_DD  (- (mean [a_l] of both-ends)) * abs delta_KG_DD ]
        ifelse delta_KB_DD > 0 [set vf_B_DD ((abs delta_KB_DD) ^ (mean [a_g] of both-ends)) ][set vf_B_DD  (- (mean [a_l] of both-ends)) * abs delta_KB_DD ]

        ifelse delta_KG_CC > 0 [set vf_G_CC ((abs delta_KG_CC) ^ (mean [a_g] of both-ends)) ][set vf_G_CC  (- (mean [a_l] of both-ends)) * abs delta_KG_CC ]
        ifelse delta_KB_CC > 0 [set vf_B_CC ((abs delta_KB_CC) ^ (mean [a_g] of both-ends)) ][set vf_B_CC  (- (mean [a_l] of both-ends)) * abs delta_KB_CC ]


        let teta_G (p_rain ^ c_p) / ((p_rain + ((1 - p_rain) ^ c_p)) ^ (1 / c_p))
        let p_bad 1 - p_rain
        let teta_B (p_bad ^ c_p) / ((p_bad + ((1 - p_bad) ^ c_p)) ^ (1 / c_p))

        let VFG_DD teta_G * vf_G_DD
        let VFB_DD teta_B * vf_B_DD

        let VFG_CC teta_G * vf_G_CC
        let VFB_CC teta_B * vf_B_CC


        ifelse VFG_CC + VFB_CC > VFG_DD + VFB_DD [
          ask end1 [
            set agreement "C"
            set trust_partner p_a
          ]
          ask end2 [
            set agreement "C"
            set trust_partner p_b
          ]
        ]
        [
          ask both-ends [
            set agreement "D"
            set score 1 / 3
          ]

        ]

      ]





    ]
    [
      ask both-ends [
        set agreement "D"
        set score 1 / 3
      ]
    ]
  ]

end

;###############################################################################################################
to final-decisions
  ask contracts [
    ask both-ends[
      let U-Cc (productivity / (productivity + [productivity] of other-end)) * factor-of-cooperation *  rain * (productivity * L + [productivity] of other-end * L)
      let U-Cd rain * productivity * (L - Lc)

      let U-Dc (productivity / (productivity + [productivity] of other-end)) * factor-of-cooperation * rain * productivity * (L + Lc - Lw) + wages * Lw
      let U-Dd rain * productivity * (L - Lw) + wages * Lw


      if decition-theory = "EU" [

        let p_C  trust_partner * U-Cc + (1 - trust_partner) * U-Cd
        let p_D  trust_partner * U-Dc + (1 - trust_partner) * U-Dd

        if-else p_C >= p_D [set Final_action "C"][set Final_action "D"]
      ]

      if decition-theory = "prospect-theory" [

        let delta_K_Cc U-Cc - ref_point
        let delta_K_Cd U-Cd - ref_point

        let delta_K_Dc U-Dc - ref_point
        let delta_K_Dd U-Dd - ref_point

        ifelse delta_K_Cc > 0 [set vf_Cc ((abs delta_K_Cc) ^ a_g)][set vf_Cc  (- a_l) * abs delta_K_Cc]
        ifelse delta_K_Cd > 0 [set vf_Cd ((abs delta_K_Cd) ^ a_g)][set vf_Cd  (- a_l) * abs delta_K_Cd]

        ifelse delta_K_Dc > 0 [set vf_Dc ((abs delta_K_Dc) ^ a_g)][set vf_Dc  (- a_l) * abs delta_K_Dc]
        ifelse delta_K_Dd > 0 [set vf_Dd ((abs delta_K_Dd) ^ a_g)][set vf_Dd  (- a_l) * abs delta_K_Dd]

        let teta_C (trust_partner ^ c_p) / ((trust_partner + ((1 - trust_partner) ^ c_p)) ^ (1 / c_p))
        let no_trust 1 - trust_partner
        let teta_D (no_trust ^ c_p) / ((no_trust + ((1 - no_trust) ^ c_p)) ^ (1 / c_p))

        let VFF_Cc teta_C * vf_Cc
        let VFF_Cd teta_D * vf_Cd

        let VFF_Dc teta_C * vf_Dc
        let VFF_Dd teta_D * vf_Dd

        ifelse VFF_Cc +  VFF_Cd >= VFF_Dc + VFF_Dd [set Final_action "C"][set Final_action "D"]


      ]


    ]
  ]
end


;###############################################################################################################
to update-SCORE-trust
    ask Comunidad with [current-partner-contract != "N"][
      if [agreement] of current-partner-contract = "D" and [Final_action] of current-partner-contract = "D"[
        ask current-partner-contract [set score 1 / 3]
      ]
      if [agreement] of current-partner-contract = "D" and [Final_action] of current-partner-contract = "C"[
        ask current-partner-contract [set score 2 / 3]
      ]
      if [agreement] of current-partner-contract = "C" and [Final_action] of current-partner-contract = "D"[
        ask current-partner-contract [set score 0]
      ]
      if [agreement] of current-partner-contract = "C" and [Final_action] of current-partner-contract = "C"[
        ask current-partner-contract [set score 3 / 3]
      ]

    ]

    ask Comunidad [set reputation ifelse-value (any? my-in-friends) [mean [trust] of my-in-friends][0]]

    if (any? friends)[
      ask friends [
        set trust precision ((1 - trust-decay) * trust + trust-decay * [score] of end2) 2
      ]


      ask friends [
        set p importance_trust * trust  + (1 - importance_trust) * [reputation] of end2

      ]


    ]
;    ask Ncontracts [
;        let f_a one-of friends with [end1 = [end1] of myself and end2 = [end2] of myself]
;        let f_b one-of friends with [end1 = [end2] of myself and end2 = [end1] of myself]
;        set  N-time-parners ([p] of f_a) * [p] of f_b
;      ]
end


;###############################################################################################################
to update-payoff

  ask contracts [
    ask both-ends[

      if [Final_action] of other-end = "D" and Final_action = "D"[
        set Wealth Wealth + rain * productivity * (L - Lw) + wages * Lw
        set past_wealth replace-item counts past_wealth (rain * productivity * (L - Lw) + wages * Lw)


        set color blue
      ]
      if [Final_action] of other-end = "D" and Final_action = "C"[
        set Wealth Wealth + rain * productivity * (L - Lc)
        set past_wealth replace-item counts past_wealth (rain * productivity * (L - Lc))
        set color white
      ]
      if [Final_action] of other-end = "C" and Final_action = "D"[
        set Wealth Wealth + rain * factor-of-cooperation * productivity * (L + Lc - Lw) + wages * Lw
        set past_wealth replace-item counts past_wealth (rain * factor-of-cooperation * productivity * (L + Lc - Lw) + wages * Lw)
        set color red
        set N_CD N_CD + 1
      ]
      if [Final_action] of other-end = "C" and Final_action = "C"[
        set Wealth Wealth + factor-of-cooperation * rain * (productivity * L)
        set past_wealth replace-item counts past_wealth (factor-of-cooperation * rain * (productivity * L))
        set color green
        set N_Contracts N_Contracts + 0.5
        ask myself [set hidden? false]
      ]
    ]

    if [final_action] of end1 = "D" or [final_action] of end2 = "D" [die]
  ]
  ask comunidad with [current-partner-contract ="N"][
    set Wealth Wealth + rain * productivity * (L - Lw) + wages * Lw
    set past_wealth replace-item counts past_wealth (rain * productivity * (L - Lw) + wages * Lw)
  ]
end


;###############################################################################################################
to update-globals-reporters
  set T_contracts T_contracts + 0.01 * N_Contracts
  set T_CD T_CD + 0.01 * N_CD
  ; nw:set-context turtle-set comunidad link-set friends                                                           ;;layaout the network
  ;  print nw:mean-path-length

   ask contracts [die]
   ask Comunidad [set current-partner-contract "N"
     set score 1 / 3
     set trust_partner 0
   ]
end


;###############################################################################################################
to GO
;profiler:start  ;;to check the computational time needed per procedude

  tick
  set N_Contracts 0
  set N_CD 0
  if Lc + Lw > 1 [                                      ;maitain L = 1
    print (Lc + Lw)
    stop]
  if decition-theory = "prospect-theory" [update-reference-point]
  make-agreements                                                     ;make agreements before rainfall
  make-rain                                                           ;rainfall
  final-decisions                                                     ;update decition to stay or leave the community after rainfall
  update-SCORE-trust                                                  ;
  update-payoff
  layout
  display                                                             ;display the families and the friendship links
  update-globals-reporters
  change-families-position
 ; global-clustering-coefficient
 ; profiler:stop          ;; stop ;;profiling
 ; print profiler:report  ;; view the results
 ; profiler:reset         ;; clear the data
  ;print [N-time-parners] of Ncontracts

  counter
end

;###############################################################################################################
to layout
  let factor ifelse-value (any? contracts)[((count contracts) ^ (1 / 2))] [1]
  ;layout-spring Comunidad with [any? contracts] contracts factor / 7 (6 * factor) factor / 5
  layout-tutte (Comunidad with [reputation > 0.5]) friends 20
  ;layout-radial Comunidad friends max-one-of Comunidad [reputation]
  ask Comunidad [set size (1 + 4 * reputation)]
end

;to global-clustering-coefficient
;  set Ccoef [nw:clustering-coefficient] of Comunidad
;end


;###############################################################################################################
to change-families-position
  if mouse-down? [
    let candidate min-one-of Comunidad [distancexy mouse-xcor mouse-ycor]
    if [distancexy mouse-xcor mouse-ycor] of candidate < 20 [
      ;; The WATCH primitive puts a "halo" around the watched turtle.
      watch candidate
      while [mouse-down?] [
        ;; If we don't force the view to update, the user won't
        ;; be able to see the turtle moving around.
        display
        ;; The SUBJECT primitive reports the turtle being watched.
        ask subject [ setxy mouse-xcor mouse-ycor ]
      ]
      ;; Undoes the effects of WATCH.  Can be abbreviated RP.
      reset-perspective
    ]
  ]

end

to counter
  set counts counts + 1
  if ticks mod 5 = 0[set counts 0]
end

to update-reference-point
  ask comunidad [
    set ref_point precision mean past_wealth 3
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
624
445
50
50
4.0
1
10
1
1
1
0
0
0
1
-50
50
-50
50
0
0
1
ticks
30.0

SLIDER
3
147
175
180
factor-of-cooperation
factor-of-cooperation
1
4
1.5
0.01
1
NIL
HORIZONTAL

SLIDER
2
193
176
226
variability-Land-productivity
variability-Land-productivity
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
2
232
174
265
trust-decay
trust-decay
0
1
0.3
0.001
1
NIL
HORIZONTAL

SLIDER
2
269
174
302
importance_trust
importance_trust
0
1
0.5
0.25
1
NIL
HORIZONTAL

BUTTON
21
104
84
137
NIL
GO
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
22
36
89
69
NIL
SETUP
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
4
308
176
341
temp_corr
temp_corr
0
0.8
0.3
0.1
1
NIL
HORIZONTAL

PLOT
636
33
836
183
rainfall
NIL
NIL
0.0
10.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot Rain"

SLIDER
5
347
177
380
p_good
p_good
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
5
387
177
420
Lc
Lc
0
1
0.2
0.1
1
NIL
HORIZONTAL

BUTTON
22
70
85
103
NIL
GO
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
3
426
175
459
Lw
Lw
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
3
468
175
501
wages
wages
0
4
4
0.1
1
NIL
HORIZONTAL

PLOT
854
199
1054
349
trust
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "if (any? friends)[plot mean [p] of friends]"

PLOT
637
196
837
346
Contracts
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot N_Contracts"
"pen-1" 1.0 0 -5298144 true "" "plot N_CD"

CHOOSER
346
451
511
496
enviromental-information
enviromental-information
"yes" "no"
1

SLIDER
10
510
182
543
var_R
var_R
0
1
0.6
0.1
1
NIL
HORIZONTAL

BUTTON
650
380
814
413
NIL
change-families-position
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
205
449
344
494
decition-theory
decition-theory
"prospect-theory" "EU"
0

CHOOSER
503
450
641
495
type_of_farmers
type_of_farmers
"risk-seeking" "risk-averse"
1

PLOT
855
33
1055
183
plot 1
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [nw:closeness-centrality] of comunidad"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Experiment_wage_sweep_PT" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>T_contracts</metric>
    <metric>mean [wealth] of comunidad</metric>
    <metric>mean [reputation] of comunidad</metric>
    <metric>mean [trust] of friends</metric>
    <enumeratedValueSet variable="factor-of-cooperation">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lw">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temp_corr">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enviromental-information">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust-decay">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_good">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importance_trust">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="wages" first="1" step="0.05" last="4"/>
    <enumeratedValueSet variable="Lc">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variability-Land-productivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decition-theory">
      <value value="&quot;prospect-theory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type_of_farmers">
      <value value="&quot;risk-seeking&quot;"/>
      <value value="&quot;risk-averse&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="correlated_enviroments_and_env_variability" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>T_contracts</metric>
    <metric>mean [wealth] of comunidad</metric>
    <metric>mean [reputation] of comunidad</metric>
    <metric>mean [trust] of friends</metric>
    <enumeratedValueSet variable="p_good">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="temp_corr" first="0" step="0.1" last="0.8"/>
    <enumeratedValueSet variable="enviromental-information">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust-decay">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wages">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lc">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="factor-of-cooperation">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importance_trust">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variability-Land-productivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lw">
      <value value="0.3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="var_R" first="0.1" step="0.05" last="0.4"/>
    <enumeratedValueSet variable="decition-theory">
      <value value="&quot;prospect-theory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type_of_farmers">
      <value value="&quot;risk-seeking&quot;"/>
      <value value="&quot;risk-averse&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sweep_pgood-Evar" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>T_contracts</metric>
    <metric>T_CD</metric>
    <metric>mean [wealth] of comunidad</metric>
    <metric>mean [reputation] of comunidad</metric>
    <metric>mean [trust] of friends</metric>
    <enumeratedValueSet variable="Lw">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importance_trust">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p_good" first="0.1" step="0.02" last="0.8"/>
    <enumeratedValueSet variable="variability-Land-productivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wages">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temp_corr">
      <value value="0"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust-decay">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_R">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enviromental-information">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="factor-of-cooperation">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lc">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_prospect_theory" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>T_contracts</metric>
    <metric>T_CD</metric>
    <metric>mean [wealth] of comunidad</metric>
    <metric>mean [reputation] of comunidad</metric>
    <metric>mean [trust] of friends</metric>
    <enumeratedValueSet variable="factor-of-cooperation">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enviromental-information">
      <value value="&quot;yes&quot;"/>
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temp_corr">
      <value value="0"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust-decay">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importance_trust">
      <value value="0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p_good" first="0.1" step="0.05" last="0.8"/>
    <enumeratedValueSet variable="wages">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decition-theory">
      <value value="&quot;prospect-theory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variability-Land-productivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_R">
      <value value="0.1"/>
      <value value="0.3"/>
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lc">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lw">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type_of_farmers">
      <value value="&quot;risk-seeking&quot;"/>
      <value value="&quot;risk-averse&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_PT_env-variability" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>T_contracts</metric>
    <metric>T_CD</metric>
    <metric>mean [wealth] of comunidad</metric>
    <metric>mean [reputation] of comunidad</metric>
    <metric>mean [trust] of friends</metric>
    <enumeratedValueSet variable="enviromental-information">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lc">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temp_corr">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_good">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust-decay">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type_of_farmers">
      <value value="&quot;risk-seeking&quot;"/>
      <value value="&quot;risk-averse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lw">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decition-theory">
      <value value="&quot;prospect-theory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="factor-of-cooperation">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importance_trust">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variability-Land-productivity">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wages">
      <value value="2.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="var_R" first="0" step="0.01" last="0.6"/>
  </experiment>
  <experiment name="experiment-LaborAlocation" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>T_contracts</metric>
    <metric>T_CD</metric>
    <metric>mean [wealth] of comunidad</metric>
    <metric>mean [reputation] of comunidad</metric>
    <metric>mean [trust] of friends</metric>
    <enumeratedValueSet variable="importance_trust">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust-decay">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enviromental-information">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wages">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variability-Land-productivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decition-theory">
      <value value="&quot;prospect-theory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temp_corr">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lw">
      <value value="0"/>
      <value value="0.0010"/>
      <value value="0.3333"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lc">
      <value value="0"/>
      <value value="0.0010"/>
      <value value="0.3333"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type_of_farmers">
      <value value="&quot;risk-averse&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="factor-of-cooperation">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p_good">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="var_R">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="RVar_vs_pgood" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>T_contracts</metric>
    <metric>T_CD</metric>
    <metric>mean [wealth] of comunidad</metric>
    <metric>mean [reputation] of comunidad</metric>
    <metric>mean [trust] of friends</metric>
    <enumeratedValueSet variable="wages">
      <value value="2.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="importance_trust">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Lw">
      <value value="0.3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p_good" first="0.2" step="0.02" last="0.8"/>
    <enumeratedValueSet variable="Lc">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="type_of_farmers">
      <value value="&quot;risk-seeking&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="decition-theory">
      <value value="&quot;prospect-theory&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="factor-of-cooperation">
      <value value="1.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trust-decay">
      <value value="0.3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="var_R" first="0.3" step="0.02" last="0.8"/>
    <enumeratedValueSet variable="variability-Land-productivity">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temp_corr">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enviromental-information">
      <value value="&quot;no&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
