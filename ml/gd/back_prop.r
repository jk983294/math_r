# =============================================
# Backpropagation (reverse-mode autodiff) demo
# Computational graph:
#
#     x ──┬──> C = x^3 ──┐
#         │              ├──> F = C * S
#     x ──┤              │
#     y ──┴──> S = x+3y ─┘
#
# Forward: compute node values.
# Backward: chain rule, propagating dF/d(node) from output to inputs.
#
# Analytic derivatives:
#   dF/dC = S                  dF/dS = C
#   dC/dx = 3 x^2              dS/dx = 1       dS/dy = 3
#   dF/dx = S * 3x^2 + C * 1
#   dF/dy = C * 3
# =============================================

# ===== Inputs =====
x <- 2
y <- 1

# ===== Forward pass =====
C <- x^3        # C = x^3
S <- x + 3 * y  # S = x + 3y
F <- C * S      # F = C * S

cat(sprintf("x = %.4f, y = %.4f\n", x, y))
cat(sprintf("C = x^3     = %.4f\n", C))
cat(sprintf("S = x+3y    = %.4f\n", S))
cat(sprintf("F = C * S   = %.4f\n\n", F))

# ===== Backward pass (reverse-mode) =====
# Seed: dF/dF = 1
dF <- 1

# Node F = C * S
#   dF/dC = S,  dF/dS = C  (times upstream dF)
dC <- dF * S
dS <- dF * C

# Node C = x^3
#   dC/dx = 3 x^2 ; accumulate into x
dC_dx <- 3 * x^2
dF_dx_from_C <- dC * dC_dx

# Node S = x + 3y
#   dS/dx = 1, dS/dy = 3 ; accumulate into x and y
dS_dx <- 1
dS_dy <- 3
dF_dx_from_S <- dS * dS_dx
dF_dy <- dS * dS_dy

# Accumulate gradients at the shared input x (two paths)
dF_dx <- dF_dx_from_C + dF_dx_from_S

cat("Backward pass (reverse-mode):\n")
cat(sprintf("  dF/dC = %.4f\n", dC))
cat(sprintf("  dF/dS = %.4f\n", dS))
cat(sprintf("  dF/dx (via C: S*3x^2) = %.4f\n", dF_dx_from_C))
cat(sprintf("  dF/dx (via S: C*1)    = %.4f\n", dF_dx_from_S))
cat(sprintf("  dF/dx (total)         = %.4f\n", dF_dx))
cat(sprintf("  dF/dy                 = %.4f\n\n", dF_dy))

# ===== Verify with analytic formulas =====
dF_dx_analytic <- S * 3 * x^2 + C
dF_dy_analytic <- C * 3

cat("Analytic check:\n")
cat(sprintf("  dF/dx = S*3x^2 + C = %.4f  (match: %s)\n",
            dF_dx_analytic, isTRUE(all.equal(dF_dx, dF_dx_analytic))))
cat(sprintf("  dF/dy = C*3        = %.4f  (match: %s)\n",
            dF_dy_analytic, isTRUE(all.equal(dF_dy, dF_dy_analytic))))

# ===== Verify with numerical (central) finite differences =====
eps <- 1e-6
F_at <- function(x, y) (x^3) * (x + 3 * y)
dF_dx_num <- (F_at(x + eps, y) - F_at(x - eps, y)) / (2 * eps)
dF_dy_num <- (F_at(x, y + eps) - F_at(x, y - eps)) / (2 * eps)

cat("\nFinite-difference check:\n")
cat(sprintf("  dF/dx (numeric) = %.6f  (match: %s)\n",
            dF_dx_num, isTRUE(all.equal(dF_dx, dF_dx_num, tolerance = 1e-4))))
cat(sprintf("  dF/dy (numeric) = %.6f  (match: %s)\n",
            dF_dy_num, isTRUE(all.equal(dF_dy, dF_dy_num, tolerance = 1e-4))))
