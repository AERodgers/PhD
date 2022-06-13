Erase all
Font size: 20
Line width: 2
Select outer viewport: 0, 10, 0, 8
Solid line
Helvetica

colours$# = {"Black", "Red", "Green", "Cyan"}
mode$# = {"MDC", "MWH", "MYN", "MDQ"}


Line width: 7
for i to 4
    Colour: colours$#[i]
    Scatter plot where (mark): "utt_slope_z", -5, 5, "utt_mean_f0_z", -3, 3, 1, "no", "o", "self$[""stim""] == mode$#[i]"
endfor


for i to 4
    for j from 1 to 4
        Line width: ((5 - j ))^2/4
        Colour: colours$#[i]
        Draw ellipse where (standard deviation): "utt_slope_z", -5, 5, "utt_mean_f0_z", -3, 3, (j) / 2, "no", "self$[""stim""] == mode$#[i]"
    endfor
endfor


Line width: 1
Draw inner box
Marks left every: 1, 0.2, "no", "yes", "no"
Marks bottom every: 1, 0.2, "no", "yes", "no"

Line width: 2
Marks left every: 1, 1, "yes", "yes", "no"
Marks bottom every: 1, 1, "yes", "yes", "no"

Text bottom: "yes", "slope (z-score)"
Text left: "yes", "f0 (z-score)"
Text top: "yes", "Distribution of Sentence modes by f0 and slope"

Black
Draw line: -5, 0, 5, 0
Draw line: 0, -3, 0, 3
