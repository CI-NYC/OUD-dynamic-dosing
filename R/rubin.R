lmtp_type <- \(x) structure(x, class = class(x[[1]]))

rubins_rules <- \(x, ...) UseMethod("rubins_rules", lmtp_type(x))

rubins_rules.lmtp <- function(lmtps, label, alpha = 0.05) {
  thetas <- purrr::map_dbl(lmtps, "theta")
  vw <- mean(purrr::map_dbl(lmtps, \(x) x$standard_error^2))
  vb <- var(thetas)
  theta <- mean(thetas)
  se <- pooled_se(vw, vb, length(thetas))
  
  data.frame(
    label = label,
    theta = theta,
    se = se,
    alpha = alpha,
    conf.low = theta + stats::qnorm(alpha / 2) * se,
    conf.high = theta + (stats::qnorm(alpha / 2) * -1) * se
  )
}

rubins_rules.lmtp_contrast <- function(lmtps, label, log = FALSE, alpha = 0.05) {
  thetas <- unlist(lapply(lmtps, \(x) x$vals$theta))
  vw <- mean(unlist(lapply(lmtps, \(x) x$vals$std.error^2)))
  vb <- var(thetas)
  theta <- mean(thetas)
  se <- pooled_se(vw, vb, length(thetas))
  
  data.frame(
    label = label,
    theta = theta,
    se = se,
    alpha = alpha,
    conf.low = if (log) {
      exp(log(theta) + stats::qnorm(alpha / 2) * se)
    } else {
      theta + stats::qnorm(alpha / 2) * se
    },
    conf.high = if (log) {
      exp(log(theta) + (stats::qnorm(alpha / 2) * -1) * se)
    } else {
      theta + (stats::qnorm(alpha / 2) * -1) * se
    }
  )
}

pooled_se <- \(vw, vb, m) sqrt(vw + vb + (vb / m))
