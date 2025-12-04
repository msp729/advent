&fras ⊡1 &args # read in the file given on stdin
⊜□ ⊸≠@\n       # split into lines
≡◇(            # for every line:
   ×⊃(
    ¬×2=@L↙1 # if the first character is L, -1, otherwise +1
  | ⋕↘1      # parse the string (except the first character) as a number
  )          # multiplied
)

⟜(°¤/+≡(⌊÷100⌵)) # store the total number of overkill rotations (rotations by more than 100 ticks)

≡(×⊃±(◿100⌵))             # mod out the overkill
⊂50↯¤⊸⧻                   # reshape & prepare for scan
◿ 100 ⊸\+                 # scan
↘1≡⊟                      # pair up
↥↥≡⊃(=0⊡0|>100˜-°⊟|>0-°⊟) # did we hit 0?
/+                        # count the 0s

+ # add the 0s to the overkill 0s
