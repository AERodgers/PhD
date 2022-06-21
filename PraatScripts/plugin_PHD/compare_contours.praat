form
    comment Contour Options
    natural f0_floor 75
    natural f0_ceiling 500
    optionmenu Smoothing 2
        option None
        option Mild (19)
        option Medium (10)
        option Intense (3)
    comment Image Options
    boolean Reset_image_window 0
    natural Font_size 14
    natural Base_line_width 6
    boolean Line_border 1

    optionmenu Line_colour 2
        option Dark Orange
        option Light Orange
        option Lilac
        option Purple
        option Black
        option Grey
endform

base_line_width
# Colour scheme: ColorBrewer2.org, photocopy, print friendy, colorblind safe
line_colour$# = {
             ... "{0.902, 0.380, 0.004}",
             ... "{0.992, 0.722, 0.388}",
             ... "{0.698, 0.671, 0.824}",
             ... "{0.369, 0.235, 0.600}",
             ... "{0.000, 0.000, 0.000}",
             ... "{0.500, 0.500, 0.500}"
             ... }
smoothing# = {0, 19, 10, 3}

@drawDecs: f0_floor, f0_ceiling, smoothing#[smoothing],
    ... reset_image_window, line_colour$#[line_colour],
    ... line_border, base_line_width, font_size

procedure drawDecs: .f0_min, .f0_max, .smoothing,
    ... .erase_all, .colour$,
    ... .border, .line_width, .font_size

    if .erase_all
        # reset Praat Picture Window
        Erase all
        Times
        Select outer viewport: 0, 6, 0, 3.5
        'font_size'
        Black
        Line width: 1
        Solid line
    endif


    .sounds# = selected#("Sound")
    .grids# = selected#("TextGrid")

    if (size(.sounds#) == size(.grids#)) and (size(.sounds#)*size(.grids#))
        for .i to size(.grids#)
            selectObject: .grids#[.i]
            .s_t = Get end time of interval: 1, 1
            .num_ints = Get number of intervals: 1
            .e_t = Get start time of interval: 1, .num_ints

            selectObject: .sounds#[.i]
            .pitch = To Pitch (ac): 0, .f0_min, 15, "no",
                                ... 0.03, 0.45, 0.01, 0.35, 0.14, .f0_max

            if .smoothing
                .temp_pitch = Smooth: .smoothing
                removeObject: .pitch
                .pitch = .temp_pitch
                selectObject: .pitch
            endif

            if .border
                Colour: "Black"
                Line width: .line_width * 1.2
                Draw logarithmic: .s_t, .e_t, .f0_min, .f0_max, "no"
            endif

            Colour: '.colour$'
            Line width: .line_width
            Draw logarithmic: .s_t, .e_t, .f0_min, .f0_max, "no"
            Remove
        endfor
    elsif size(.sounds#) != size(.grids#)
        exitScript: "Mismatching number of Sound and TextGrid objects selected"
        ... + newline$
    else
        exitScript: "Either no Sound object or no TextGrid objects selected"
        ... + newline$ + "or no appropriate objects selected." + newline$
    endif


    # Garnishing
    if .erase_all
        Black
        Line width: 1
        Draw inner box
        Marks left every: 1, 0.1, "yes", "yes", "yes"
        Text left: "yes", "log %f_0"

        Axes: 0, 1, 0, 1
        Marks bottom every: 0.01, 10, "yes", "yes", "no"
        Text bottom: "yes", "time (\%  of utterance)"
    endif

    selectObject: .sounds#
    plusObject: .grids#
endproc
