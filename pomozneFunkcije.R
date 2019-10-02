# Functions ------------------------------------------------------------

## Calculate payment (L. Collado Torres)
pay <-
        function(principal,
                 interest,
                 duration,
                 payfreq,
                 firstpay,
                 compoundfreq) {
                r <- interest / (100 * 12 / compoundfreq)
                
                if (firstpay > 1) {
                        principal <- principal * (1 + r) ^ ((firstpay - 1) / compoundfreq)
                        duration <- duration - (firstpay - 1) / 12
                }
                
                payment <-
                        principal * r / (1 - (1 + r) ^ (-duration * 12 / compoundfreq)) * payfreq / compoundfreq
                res <- list(r = r,
                            payment = payment,
                            principal = principal)
                return(res)
        }

## Amortization table (L. Collado Torres)
amort <-
        function(principal,
                 interest,
                 duration,
                 payfreq,
                 firstpay,
                 compoundfreq) {
                pay <-
                        pay(principal,
                            interest,
                            duration,
                            payfreq,
                            firstpay,
                            compoundfreq)
                data <- data.frame(month = seq(0, duration * 12))
                data$payment <- 0
                data$payment[(data$month - firstpay) >= 0 &
                                     (data$month - firstpay) %% payfreq == 0] <-
                        pay$payment
                i <- which(data$payment != 0)
                i <- i[length(i)]
                data$payment[i] <- 0
                data$payment[i] <-
                        pay$payment * (duration - (firstpay - 1) / 12) * 12 / payfreq - sum(data$payment)
                data$totalPayed <- cumsum(data$payment)
                
                data$principal <- NA
                data$principal[1] <- principal
                idx <-
                        (data$month - firstpay) >= 0 &
                        (data$month - firstpay) %% compoundfreq == 0
                idx.pr <-
                        which(idx)[-length(idx)] + compoundfreq - 1
                if (any(idx.pr > max(data$month))) {
                        idx.pr <- idx.pr[-which(idx.pr > max(data$month))]
                }
                
                if (firstpay > 1) {
                        data$principal[firstpay] <- pay$principal
                }
                data$principal[idx.pr] <-
                        (1 + pay$r) ^ seq_len(length(idx.pr)) * pay$principal - ((1 + pay$r) ^
                                                                                         seq_len(length(idx.pr)) - 1) / pay$r * pay$payment * compoundfreq / payfreq
                data$principal[nrow(data)] <- 0
                
                return(data)
        }

# simulation
cenaInvesticije <- function(loanDuration,
                            euribor,
                            bankInterest,
                            investmentEstimate,
                            priceIncrease,
                            currentFunds,
                            yearlySavings) {
        # error handilng
        if (!dplyr::between(priceIncrease, 0, 1)) {
                stop("Price increase should be between 0 and 100")
        }
        
        if (!dplyr::between(bankInterest, 0, 100)) {
                stop("Bank interest should be between 0 and 100")
        }
        
        if (!dplyr::between(euribor,-1, 100)) {
                stop("Euribor interest should be between -1 and 100")
        }
        
        if (!dplyr::between(loanDuration, 2, 30)) {
                stop("Loan duration should be between 2 and 30 years")
        }
        
        if (bankInterest == 0) {
                stop("I wish my bank would give me 0 interest rate :)")
        }
        
        # years
        leta <- seq(
                from = lubridate::year(Sys.Date()),
                to = lubridate::year(Sys.Date()) + 6,
                by = 1
        )
        # matrix of results
        result <- matrix(
                0,
                nrow = length(leta),
                ncol = 7,
                dimnames = list(
                        c(1:length(leta)),
                        c(
                                'startYear',
                                'funds',
                                'investment',
                                'loan',
                                'interests',
                                'payment',
                                'totalInvestment'
                        )
                )
        )
        # interests
        interest <- euribor + bankInterest  
        
        # price increase
        prices <- if (priceIncrease == 0) {
                rep(1, times = length(leta))
        } else{
                seq(
                        from = 1 ,
                        to = 1 + priceIncrease * (length(leta) - 1),
                        by = priceIncrease
                )
        }
        # cost of investment
        investment <-
                investmentEstimate * prices 
        
        savings <- rep(yearlySavings, times = length(leta) - 1)
        funds <- c(currentFunds,
                   currentFunds + cumsum(savings))
        # principal
        principal <- investment - funds 
        
        for (leto in seq_along(leta)) {
                if (funds[leto] < investment[leto]) {
                        loanData <- amort(
                                principal = principal[leto],
                                interest = interest,
                                duration = loanDuration,
                                payfreq = 1,
                                firstpay = 1,
                                compoundfreq = 1
                        )
                        result[leto, 1] <- leta[leto]
                        result[leto, 2] <- funds[leto]
                        result[leto, 3] <- investment[leto]
                        result[leto, 4] <- principal[leto]
                        result[leto, 5] <-
                                max(loanData$totalPayed) - principal[leto]
                        result[leto, 6] <- max(loanData$payment)
                        result[leto, 7] <-
                                investment[leto] + (max(loanData$totalPayed) - principal[leto])
                }else{
                        result[leto, 1] <- leta[leto]
                        result[leto, 2] <- funds[leto]
                        result[leto, 3] <- investment[leto]
                        result[leto, 4] <- 0
                        result[leto, 5] <- 0
                        result[leto, 6] <- 0
                        result[leto, 7] <- investment[leto]
                }
                
        }
        
        return(as.data.frame(result))
}