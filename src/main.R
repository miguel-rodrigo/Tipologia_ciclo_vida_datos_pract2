library(data.table)
library(caret)

data <- fread('data/train.csv')


#### FIX DATA FORMAT ####
# data[Embarked == "", Embarked := "S"]
data[, `:=`(
  Survived = as.factor(Survived),
  Embarked = as.factor(Embarked),
  Sex = as.factor(Sex),
  Pclass = as.factor(Pclass),
  PassengerId = NULL,  # No necesitmos este ya que es un índice autoincremental
  Name = NULL
)]


#### CHECK MISSING DATA ####
data[, lapply(.SD, function(x) {sum(is.na(x) | x=="")/.N} )]

# Hay casi un 80% de datos faltantes en la cabina, con lo que es probable que se elimine. Hay casi un 20 en la edad,
# por lo que se testeará para ver cuál es la mejor solución.

# En lugar de ir paso a paso, se va mejor variable a variable.

# Primero, categóricas

#### CABIN ####
# 1. comprobar los valores únicos
# 2. Hay muchísimos, hay que hacer algo
# 3. Usar la primera letra
# 4. Usar el número
# 5. Comprobar dependencias usando las filas donde no falta el valor
# 6. En base a lo obtenido en el punto 5, se decide si imputar o si eliminar, o tal vez dejar como está y poner un valor
#   en las filas que faltan.

# 1.
uniqueN(data$Cabin)

# 3., 4.
data[, `:=`(
  cabin_letter = substr(Cabin, 1, 1),
  
  # remove first character, split by space, unlist and take 1st element:
  cabin_number = as.integer(unlist(lapply(strsplit(substr(Cabin, 2, 999), " "), `[`, 1)))
)]

# 5.
data_cabin <- data[Cabin != "", .(Survived, cabin_letter, cabin_number)]

plot(Survived ~ cabin_number, data=data_cabin, breaks=50)
# No parece que haya influencia. Algunas barras tienen poca frecuencia (son estrechas) y altas, pero no tienen significación.
# En general, no parece que haya una tendencia con respecto al número.

cabin.aov <- aov(cabin_number ~ Survived, data=data_cabin)
summary(cabin.aov)
# Efectivamente, el test no encuentra una relación significativa entre el número y la probabilidad.

chisq.test(data_cabin$Survived, data_cabin$cabin_letter)
# La letra tampoco parece que influya, pero está tan cerca que no parece apropiado rechazarla directamente. En ese caso
#   es importante tener en cuenta que hay muchísimos valores faltantes...

data[, Cabin := NULL]

#### TICKET ####
# 1. ¿Sirve de algo? No. Ea po fuera
# 2. Los tickets iguales son de gente junta -> Número de tickets iguales como feature
data[, num.together := .N, by=Ticket]
data[, Ticket := NULL]

# Hay más infor en los tickets (a4,a5, pc, line..., pero se va a dejar así de momento)

#### Pclass ####
# 1. Ver número de valores únicos
# 2. Ver dependencia con la variable salida
data[, unique(Pclass)]
barplot(data[, sum(Survived), by=Pclass][order(Pclass), V1])
chisq.test(data$Pclass, data$Survived)


#### SEX ####
# 1. Valores únicos
# 2. Dependencia con la salida
data[, unique(Sex)]  # Sólo hay dos, había que comprobar por los valores vacíos etc...
chisq.test(data$Sex, data$Survived)


# Ahora, las numéricas


#### AGE ####
# 1. Ver distribución. ¿Hay outliers?
# 2. Comprobar dependencia usando las filas que hay
# 3. ¿Modelo sin edad vs modelo con edad (usando la media)?
# 4. Estudiar métodos de imputación:
#     a. Media
#     b. Mediana
#     c. Regresión lineal en base al resto de atributos
boxplot(data$Age)  # No parece que haya outliers
plot(data[order(Age), Age]) # Hay bastantes menos niños y más gente mayor, pero todo parece coherente.

# Sí que parece que hay alguno con respecto a la tarifa
age_fare <- lm(Age ~ Fare, data=data)
plot(age_fare, which=c(2,1))

data[c(117, 631, 852)]
# La desviación no parece tal como para justificar su eliminación.

age_surv_aov <- aov(Survived ~ Age, data = data[!is.na(Age)])
# Sí que hay dependencia, ahora qué?

feats <- setdiff(names(data), c("cabin_letter", "cabin_number"))
age_vs_noage <- test_bench(
  na.omit(data[, feats, with=F]),
  na.omit(data[, setdiff(feats, "Age"), with=FALSE]),
  "Survived"
)
test_bench.assess(age_vs_noage)
# According to the results, there is no significant difference between using age column and not using it

imputed_age <- copy(data[, feats, with=FALSE])
mean_age <- mean(data$Age, na.rm = TRUE)
imputed_age[is.na(Age), Age := mean_age]
noage_vs_mean_age <- test_bench(
  age_vs_noage[[2]],
  na.omit(imputed_age),
  "Survived"
)
test_bench.assess(noage_vs_mean_age)

# Tenemos un volumen de datos muy limitado. En nuestro caso, entrenar con más datos tiene un beneficio mayor
#   que el sesgo introducido por los datos falsos. Probemos otro método.
random_age <- copy(data[, feats, with=FALSE])
sd_age <- sd(data$Age, na.rm = TRUE)
random_age[is.na(Age), Age := rnorm(1, mean_age, sd_age)]

mean_age_vs_rng_age <- test_bench(
  age_vs_mean_age[[2]],
  na.omit(random_age),
  "Survived"
)
test_bench.assess(mean_age_vs_rng_age)
# Da igual la media que un número aleatorio...Ahora con modelos lineales

lm1 <- lm(Age ~ Pclass + SibSp, data=na.omit(data[, .(Age, Pclass, SibSp)]))
lm2 <- lm(Age ~ Pclass + SibSp, data=na.omit(data[, .(Age, Pclass=as.integer(Pclass), SibSp)]))

age_lm1 <- copy(data[, feats, with=FALSE])
age_lm2 <- copy(data[, feats, with=FALSE])

age_lm1[is.na(Age), Age := predict(lm1, .SD)]
age_lm2[is.na(Age), Age := predict(lm2, .SD[,.(Pclass=as.integer(Pclass), SibSp)])]

age_lm1_vs_lm2 <- test_bench(age_x1, age_x2, "Survived")
test_bench.assess(age_lm1_vs_lm2)  # Dan lo mismo...

age_lm1_vs_random <- test_bench(
  mean_age_vs_rng_age[[2]],
  a[[1]],
  "Survived"
)
test_bench.assess(age_lm1_vs_random)  # Como cabía esperar, no hay diferencias...

# Conclusión: Aunque en las investigaciones, hay gente que hace de todo, es indiferente en la realidad
# Nos vamos a quedar con el aleatorio.
data_fix <- random_age


#### SIBSP ####
# 1. Ver valores únicos
# 2. Ver distribución
# 3. Ver relaciones: ¿merece más la pena ponerla como binaria?
data_fix[, unique(SibSp)]
plot(density(data_fix$SibSp))




#### PARCH ####
# ...lo mismo...


#### FARE ####
# 1. Ver distribución: ¿outliers?
# 2. Ver relaciones


# Hacer lista de versiones de los datos
data_fix_age <- copy(data)
data_fix_age[is.na(Age), Age := median(data[!is.na(Age), Age])]
# data_fix_age[is.na(Age), Age := -999]



no_nas1 <- data[, -c("Age", "Cabin")]
no_nas2 <- data_fix_age[, -c("Cabin")]



# OOB assesment
print(model)
