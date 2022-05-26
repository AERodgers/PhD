cur_selected# = selected#()
sound = selected("Sound")
selectObject: sound
start_t = Get start time
end_t = Get end time
pitch = To Pitch (ac): 0, 75, 15, "no", 0.03, 0.45, 0.01, 0.35, 0.14, 600
mean = Get mean: 0, 0, "Hertz"
removeObject: pitch
selectObject: sound
manip = noprogress To Manipulation: 0.01, 75, 600
Edit
editor: manip 
Move cursor to: start_t
Move start of selection by: end_t
Remove pitch point(s)
Add pitch point at: (end_t - start_t) / 2, mean 
endeditor
newsound = Get resynthesis (overlap-add)
removeObject: manip
selectObject: newsound
Edit
selectObject: cur_selected#





