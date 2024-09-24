
winsorize <- function(rawdata){
  xx = data.frame(
    rawdata = rawdata,
    data.z = scale(rawdata)
  )
  
  ##calculate cut-off values
  upr = mean(xx$rawdata, na.rm = T)+3*sd(xx$rawdata, na.rm = T)
  lwr = mean(xx$rawdata, na.rm = T)-3*sd(xx$rawdata, na.rm = T)
  
  cat("winsorized upr", length(which(xx$rawdata > upr)), "\n")
  cat("winsorized lwr", length(which(xx$rawdata < lwr)), "\n")
  
  xx$rawdata[which(xx$rawdata > upr)] = upr
  xx$rawdata[which(xx$rawdata < lwr)] = lwr
  
  data_w = xx$rawdata
  
  # print check
  if (length(data_w[which(xx$data.z >= 3)]) != 0 && mean(data_w[which(xx$data.z >= 3)]) <= upr) {
    print("successfully winsorized upr")
  } else {
    print("nothing winsorized upr")
  }
  
  if (length(data_w[which(xx$data.z <= -3)]) != 0 && mean(data_w[which(xx$data.z <= -3)]) >= lwr) {
      print("successfully winsorized lwr")
    } else {
      print("nothing winsorized lwr")
    }
#   } else{
#     print("nothing winsorized")
#   }
# # && mean(data_w[which(xx$data.z >= 3)]) <= upr)
  return(data_w)
}
