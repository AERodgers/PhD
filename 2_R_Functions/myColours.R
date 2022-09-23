#123456789#123456789#123456789#123456789#123456789#123456789#123456789#123456789
# author: "Antoin Eoin Rodgers"
# date: '2022-05-06'
# Bank of personal colour settings
#
## set colours
##
require("RColorBrewer")
mode_colours <- c("MDC" = brewer.pal(8, "Dark2")[3],
                  "WHQ" = brewer.pal(8, "Dark2")[2],
                  "MYN" = brewer.pal(8, "Dark2")[1],
                  "MDQ" = brewer.pal(8, "Dark2")[4])


pitch_accent_colours <- c("H*"     = brewer.pal(8, "Dark2")[1],
                          "L*H"    = brewer.pal(8, "Dark2")[3],
                          "^[L*]H" = brewer.pal(8, "Dark2")[5],
                          ">H*"    = brewer.pal(8, "Dark2")[4],
                          "L*^[H]" = brewer.pal(8, "Dark2")[2],
                          "^[L*H]" = brewer.pal(8, "Dark2")[6],
                          "L*"     = brewer.pal(8, "Dark2")[7],
                          "(*)"    = brewer.pal(8, "Dark2")[8])


# nuc_contour_colours_h_reg  <- c("H* L%"     = brewer.pal(8, "Set2")[5],
#                                 ">H* L%"    = brewer.pal(8, "Set2")[4],
#                                 "^[L*]H L%" = brewer.pal(8, "Set2")[6],
#                                 "L*H L%"    = brewer.pal(8, "Set2")[1],
#                                 "L*^[H] L%" = brewer.pal(8, "Set2")[8],
#                                 "^[L*H] L%" = brewer.pal(8, "Set2")[7],
#                                 "L*^[H L]%" = brewer.pal(8, "Set2")[2],
#                                 "^[L*H L]%" = brewer.pal(8, "Set2")[3],
#                                 "L*H %"     = brewer.pal(8, "Set2")[3],
#                                 "L*^[H] %" = brewer.pal(8, "Set2")[2],
#                                 "^[L*H] %" = brewer.pal(8, "Set2")[7])

nuc_contour_colours <- c(
    "L*H %"   = brewer.pal(8, "Dark2")[3],
    "L*H L%" = brewer.pal(8, "Dark2")[7],
    ">H* L%"  = brewer.pal(8, "Dark2")[4],
    "H* L%"   = brewer.pal(8, "Dark2")[1],
    "L*H H%" = brewer.pal(8, "Dark2")[2],
    "L*H HL%" = brewer.pal(8, "Dark2")[5]
)

fin_phon_colours <- c(
    "%"   = brewer.pal(8, "Dark2")[3],
    "L%" = brewer.pal(8, "Dark2")[7],
    "H%" = brewer.pal(8, "Dark2")[2],
    "HL%" = brewer.pal(8, "Dark2")[8]
)

