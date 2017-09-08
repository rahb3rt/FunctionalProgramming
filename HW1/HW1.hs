fToC x = (x - 32) * 5/9

cToF x = x * 9/5 + 32

banger s = "!" ++ s ++ "!"

saywhat s = s ++ s

tallEnough x y = if (x * 12) + y > 50 then "True" else "False"

boxVolume x y z = x * y * z

max3 x y z = max (max x y) z 

stutter x = head x : head x : head x : x

ekzer x = head x : "x" ++ tail x
