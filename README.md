```{r, echo=FALSE, message=FALSE}
#cleaning data
library(readxl)
library(dplyr)
bonds_data <- read_xlsx("~/Desktop/bonds_data.xlsx")


bonds_data$issue <- as.POSIXct(bonds_data$`issue date`, format="%m/%d/%Y")
issue <- as.Date(bonds_data$issue)

bonds_data$maturity <- as.POSIXct(bonds_data$`maturity date`, format="%m/%d/%Y")
maturity<- as.Date(bonds_data$maturity)

bonds_data$term <- as.numeric(round((bonds_data$maturity - bonds_data$issue)/365,2))
bonds_data <- arrange(bonds_data, by=term)
bonds_data$ttm <- as.numeric(round((
  bonds_data$maturity - 
    as.POSIXct(Sys.Date(),format="%Y-%m-%d"))/(365),2))
bonds_data <- bonds_data[,-c(3,4)]
bonds_data <- bonds_data[c(1,2,13,14,15,16,12,11,10,9,8,7,6,5,4,3)]

bonds_data <- subset(bonds_data, term <= 30)
bonds_data <- subset(bonds_data, ttm <= 5.5)
#View(bonds_data)
sel_bonds1 <- bonds_data[7:15, ]
sel_bonds2 <- bonds_data[17:18, ]
sel_bonds <- arrange(rbind(sel_bonds1, sel_bonds2), by=ttm)
#View(sel_bonds)



```

```{r, echo=FALSE, message=FALSE}
library(jrvFinance)
prices <- sel_bonds[, 7:16]
#View(prices)
prices_t <- t(prices)
#View(prices_t)
issue <- as.Date(sel_bonds$issue)
maturity <- as.Date(sel_bonds$maturity)
coupon <- sel_bonds$coupon/100

second <- "2020-01-02"
third <- "2020-01-03"
sixth <- "2020-01-06"
seventh <- "2020-01-07"
eighth <- "2020-01-08"
nineth <- "2020-01-09"
tenth <- "2020-01-10"
thirteenth <- "2020-01-13"
fourteenth <- "2020-01-14"
fifteenth <- "2020-01-15"

settle_dates <- c(second, third, sixth, seventh, eighth, nineth, tenth, thirteenth,
                  fourteenth, fifteenth)



ytm1 = vector()
ytm2 = vector()
ytm3 = vector()
ytm4 = vector()
ytm5 = vector()
ytm6 = vector()
ytm7 = vector()
ytm8 = vector()
ytm9 = vector()
ytm10 = vector()


```

```{r, echo=FALSE, message=FALSE}
i = 1
j = 1
#1
while(i < 12)
{
  if(i == 1)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm1 <- c(ytm1, y)
      j = j + 1
    }
  }
  i = i + 1
  
}

```

```{r, echo=FALSE, message=FALSE}
i = 2
j = 1
#2
while(i < 12)
{
  if(i == 2)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm2 <- c(ytm2, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm2)
```

```{r, echo=FALSE, message=FALSE}
i = 3
j = 1
#3
while(i < 12)
{
  if(i == 3)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm3 <- c(ytm3, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm3)
```
```{r, echo=FALSE, message=FALSE}
i = 4
j = 1
#4
while(i < 12)
{
  if(i == 4)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm4 <- c(ytm4, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm4)
```
```{r, echo=FALSE, message=FALSE}
i = 5
j = 1
#5
while(i < 12)
{
  if(i == 5)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm5 <- c(ytm5, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm5)
```
```{r, echo=FALSE, message=FALSE}
i = 6
j = 1
#6
while(i < 12)
{
  if(i == 6)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm6 <- c(ytm6, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm6)
```
```{r, echo=FALSE, message=FALSE}
i = 7
j = 1
#7
while(i < 12)
{
  if(i == 7)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm7 <- c(ytm7, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm7)
```
```{r, echo=FALSE, message=FALSE}
i = 8
j = 1
#8
while(i < 12)
{
  if(i == 8)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm8 <- c(ytm8, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm8)
```
```{r, echo=FALSE, message=FALSE}
i = 9
j = 1
#9
while(i < 12)
{
  if(i == 9)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm9 <- c(ytm9, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm9)
```
```{r, echo=FALSE, message=FALSE}
i = 10
j = 1
#10
while(i < 12)
{
  if(i == 10)
  {
     while(j < 11)
    {
      y <- bond.yield(settle=settle_dates[j], mature=maturity[i], coupon=coupon[i], 
                      price=prices_t[j,i])
      ytm10 <- c(ytm10, y)
      j = j + 1
    }
  }
  i = i + 1
  
}
#print(ytm10)


```

```{r, echo=FALSE, message=FALSE}
library(lme4)
library(ggplot2)
groups <- c(ytm1, ytm2, ytm3, ytm4,ytm5,ytm6,ytm7,ytm8,ytm9,ytm10)
ytm <- round(rbind(ytm1, ytm2, ytm3, ytm4,ytm5,ytm6,ytm7,ytm8,ytm9,ytm10),4)
ytm <- as.data.frame(ytm)
rownames(ytm) <- c("bond1", "bond2", "bond3", "bond4", "bond5", "bond6", "bond7", "bond8", "bond9", "bond10")
knitr::kable(ytm, caption = "Yields to Maturity")

ttm <- factor(c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5))

ggplot(ytm, aes(x = ttm)) + 
  geom_line(aes(y = ytm[,1], group = 1, color = "Jan2")) +
  geom_line(aes(y = ytm[,2], group = 1, color = "Jan3")) +
  geom_line(aes(y = ytm[,3], group = 1, color = "Jan6")) +
  geom_line(aes(y = ytm[,4], group = 1, color = "Jan7")) +
  geom_line(aes(y = ytm[,5], group = 1, color = "Jan8")) +
  geom_line(aes(y = ytm[,6], group = 1, color = "Jan9")) +
  geom_line(aes(y = ytm[,7], group = 1, color = "Jan10")) +
  geom_line(aes(y = ytm[,8], group = 1, color = "Jan13")) +
  geom_line(aes(y = ytm[,9], group = 1, color = "Jan14")) +
  geom_line(aes(y = ytm[,10], group = 1, color = "Jan15")) +
  labs(x = "Time to Maturity", y = "Yields")

```

\hfill
     (b)\hfill
```{r, eval=FALSE}
spot <- vector()
for(i in 2:5){
  Calculate the spot rates for the first bond, which matures in 0.5 years, 
  using the formula:
    spot rate = log((100+coupon/2)/price)/time_to_maturity
  Then get the lists of time_to_maturity and cashflows of the 2nd, 3rd, 4th, 
  and 5th bonds
  for(j in 1:9)
  {
    Calculate the last discounted cashflow of each bond, saved as "denominator"
  }
  Then spot_rate = log(cashflow/"denominator")/time_to_maturity
}

for(i in 1:5)
{
  Again, calculate the last discounted cashflow of each bond from the 
  6th to the 10th bond and the spot rates
}
```

5. \hfill
```{r, echo=FALSE, message=FALSE}
five_bonds <- sel_bonds[7:11,]
#View(five_bonds)
fifty_prices <- five_bonds[,7:16]
#View(fifty_prices)
#first_nine_dates <- as.numeric(fifty_prices[,1:9])
#last_nine_dates <- as.numeric(fifty_prices[,2:10])

ln_r1 <- vector()
ln_r2 <- vector()
ln_r3 <- vector()
ln_r4 <- vector()
ln_r5 <- vector()

#bond1
i = 1
while(i < 10)
{
  log_r<-log(fifty_prices[1,i+1] / fifty_prices[1,i])
  ln_r1 = c(ln_r1, log_r)
  i = i + 1
}
ln_r1<-round(as.numeric(ln_r1),7)
#print(ln_r1)
```

```{r, echo=FALSE, message=FALSE}
#bond2
i = 1
while(i < 10)
{
  log_r<-log(fifty_prices[2,i+1] / fifty_prices[2,i])
  ln_r2 = c(ln_r2, log_r)
  i = i + 1
}
ln_r2<-round(as.numeric(ln_r2),7)
```


```{r, echo=FALSE, message=FALSE}
#bond3
i = 1
while(i < 10)
{
  log_r<-log(fifty_prices[3,i+1] / fifty_prices[3,i])
  ln_r3 = c(ln_r3, log_r)
  i = i + 1
}
ln_r3<-round(as.numeric(ln_r3),7)
```

```{r, echo=FALSE, message=FALSE}
#bond4
i = 1
while(i < 10)
{
  log_r<-log(fifty_prices[4,i+1] / fifty_prices[4,i])
  ln_r4 = c(ln_r4, log_r)
  i = i + 1
}
ln_r4 <-round(as.numeric(ln_r4),7)
```

```{r, echo=FALSE, message=FALSE}
#bond5
i = 1
while(i < 10)
{
  log_r<-log(fifty_prices[5,i+1] / fifty_prices[5,i])
  ln_r5 = c(ln_r5, log_r)
  i = i + 1
}
ln_r5<-round(as.numeric(ln_r5),7)
```

```{r, echo=FALSE, message=FALSE}
log_ret <- t(rbind(ln_r1, ln_r2, ln_r3, ln_r4, ln_r5))
colnames(log_ret) <- c("bond1", "bond2", "bond3", "bond4", "bond5")
knitr::kable(log_ret, cap="Matrix of Log-Returns")
cov_lr <- cov(log_ret)
colnames(cov_lr) <- c("bond1", "bond2", "bond3", "bond4", "bond5")
rownames(cov_lr) <- c("bond1", "bond2", "bond3", "bond4", "bond5")
knitr::kable(cov_lr,cap="Covariance Matrix of Log-Returns")

```

6. \hfill
```{r, echo=FALSE, message=FALSE}
eigv_lr <- eigen(cov_lr)
eval_lr <- eigv_lr$values
evec_lr <- eigv_lr$vectors
knitr::kable(eval_lr, cap="Eigenvalues for Covariance Matrix of Log-Return")
knitr::kable(evec_lr, 
             caption = "Eigenvectors for Covariance Matrix of Log-Return")

```
