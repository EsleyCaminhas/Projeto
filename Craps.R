
n_dice_sum = function(n){
  twodiceroll = sample(c(1,2,3,4,5,6), n, replace = T)
  sumdices = sum(twodiceroll)
  return(sumdices)
}

craps_1 = function(){
  d1 = n_dice_sum(2)
  
  if (d1 == 7 | d1 == 11){
    return("V")
  } else {
    if (d1 == 2 | d1 == 3 | d1 == 12){
      return("D")
    } else {
      return(d1)
    }
  }
}

n = 0
v = 0
d = 0

while (n < 10000){
  a = craps_1()
  if (a == "V"){
    v = v + 1 
    n = n + 1
  } else {
    if (a == "D"){
      d = d + 1
      n = n + 1
    } else {
      while(T){
        b = n_dice_sum(2)
        if (a == b){
          v = v + 1
          n = n + 1
          break
        } else {
          if (b == 7){
            d = d + 1
            n = n + 1
            break
          }
        }
      }
    }
  }
}

