"
Variable	Description					Codes/Values
1		OBS Identification Code				1-412

2		ME Mammograph Experience	THIS IS THE RESPONSE
 0 = Never
 1 = Within One Year
 2 = Over One Year Ago

3 SYMPT	You do not need a mamogram unless	you develop symptoms
1 = Strongly Agree
2 = Agree
3 = Disagree
4 = Stongly Disagree

4		PB Perveived benefit of mammography*		5 - 20

5		HIST Mother or Sister with a history of breast cancer
0 = No, 1 = Yes

6		BSE Has anyone taught you how to examine your own breasts: that is BSE
0 = No, 1 = Yes

7		DETC How likely is it that a mamogram could find a new case of breast cancer
1= Not likely
2 = Somewhat likely
3 = Very likely
"

mammograph_data <- read.table("data/meexp.dat", header=FALSE)
mammograph_data$V1 <- NULL
names(mammograph_data) <- c("ME", "SYMPT", "PB", "HIST", "BSE", "DETC")

mammograph_data$ME <- factor(mammograph_data$ME, ordered = TRUE)
levels(mammograph_data$ME) <- c("Never", "Within One Year", "Over One Year Ago")

mammograph_data$SYMPT <- factor(mammograph_data$SYMPT, ordered = TRUE)
levels(mammograph_data$SYMPT) <-
  c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree")

mammograph_data$HIST <- ifelse(mammograph_data$HIST == 1,TRUE,FALSE)
mammograph_data$HIST <- as.logical(mammograph_data$HIST)

mammograph_data$BSE <- ifelse(mammograph_data$BSE == 1,TRUE,FALSE)
mammograph_data$BSE <- as.logical(mammograph_data$BSE)


mammograph_data$DETC <- factor(mammograph_data$DETC, ordered = TRUE)
levels(mammograph_data$DETC) <-
  c("Not likely", "Somewhat likely", "Very likely")
