# 线性规划
# argmin 2 * x + 3 * y  which s.t.
#  2 * x - y >= 0
#  -1 * x + 2 * y >= 3
#  3 * x - y <= 13
boot::simplex(
  a = c(2, 3),                                      # 目标函数系数
  A1 = rbind(c(3, -1)), b1 = 13,                    # <=约束
  A2 = rbind(c(2, -1), c(-1, 2)), b2 = c(0, 3),     # >=约束，所有自变量非负约束为默认
  A3 = NULL, b3 = NULL)                             # =约束
