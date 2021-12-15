recruit <- ts(scan("recruit.txt"),f=12, start =1950)
plot(recruit)
acf(recruit, 50)

