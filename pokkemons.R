# Pokemon Case Study : 

pokemon = read.csv("C:\Users\Anjali\Desktop\pokemon.csv") 
View(pokemon)
ncol(pokemon)
nrow(pokemon)
table(pokemon$is_legendary)
table(pokemon$generation)
table(pokemon$type1)
table(pokemon$hp)
min(pokemon$hp)
max(pokemon$hp)
min(pokemon$speed)
max(pokemon$speed)
is.na(pokemon$abilities)
sum(is.na(pokemon$abilities))
colnames(pokemon)
colnames(pokemon) == 'type1'
colnames(pokemon)[colnames(pokemon)== 'type1']<-"primary"
colnames(pokemon)[colnames(pokemon)== 'type1']<-"secondary"
# To apply filters :

library(dplyr)

pokemon %>% filter(primary=="grass") -> grass
View(grass)

max(grass$speed)
mean(grass$speed)
# visualization :

library(ggplot2)

ggplot(data =pokemon, aes(x=hp))+ geom_histogram(fill="palegreen")
# How much Pokemons are legendary
ggplot(data=grass, aes(x=is_legendary)) + geom_bar()

pokemon %>% filter(primary=="fire") -> fire_pokemon
View(fire_pokemon)

# Mean.Max,Min :

min(fire_pokemon$speed)
max(fire_pokemon$speed)

mean(fire_pokemon$sp_attack)
mean(fire_pokemon$sp_defense)

ggplot(data = fire_pokemon, aes(x=generation)) +geom_bar()
ggplot(data = fire_pokemon,aes(x=generation, fill=as.factor(generation))) + geom_bar()

ggplot(data=fire_pokemon, aes(x=secondary)) +geom_bar()
ggplot(data=fire_pokemon, aes(x=secondary,fill=secondary)) + geom_bar()

ggplot(data=fire_pokemon, aes(x=sp_attack, y=sp_defense)) + geom_point(col="green")
ggplot(data=fire_pokemon, aes(x=height_m, y=weight_kg)) + geom_point(col="green")

pokemon %>% filter(pokemon$primary=="water")-> water_pokemon
View(water_pokemon)

min(water_pokemon$speed)
max(water_pokemon$speed)

mean(water_pokemon$sp_attack)
mean(water_pokemon$sp_defense)

ggplot(data=water_pokemon, aes(x=against_ice)) + geom_histogram()
ggplot(data=water_pokemon, aes(x=against_poison)) + geom_histogram()
ggplot(data=water_pokemon, aes(x=against_grass)) + geom_histogram()

pokemon %>% filter(pokemon$primary=="psychic") -> psychic_pokemon

View(pyschic_pokemon)

min(pyschic_pokemon$sp_attack)
max(pyschic_pokemon$sp_defense)

ggplot(data = pyschic_pokemon, aes(x=base_total)) + geom_histogram(fill="palegreen4")
ggplot(data = pyschic_pokemon, aes(x=as.factor(generation), y=sp_defense)) + geom_boxplot(fill="palegreen4")
ggplot(data = pyschic_pokemon, aes(x=is_legendary)) + geom_bar()

# Density Plot :
library(dplyr)
library(gridExtra)
density_hp <- ggplot(data=pokemon, aes(hp)) + geom_density(col="white",fill="pink", alpha=0.8) + ggtitle("Density Plot of HP")
density_speed <- ggplot(data=pokemon, aes(speed)) + geom_density(col="white", fill="darkorchid", alpha=0.8) + ggtitle("Density Plot of Speed Characterstics")
density_attack <- ggplot(data=pokemon, aes(attack)) + geom_density(col="white", fill="orange", alpha=0.7) + ggtitle("Density Plot of Attack Characterstics")
density_defense <- ggplot(data=pokemon, aes(defense)) + geom_density(col="white", fill="firebrick", alpha=0.7) + ggtitle("Density Plot of Defense Characterstics")
density_height <- ggplot(data=pokemon, aes(height_m)) + geom_density(col="white", fill="slateblue1", alpha=0.8) + ggtitle("Density Plot of Height (m) ")
density_weight <- ggplot(data=pokemon, aes(weight_kg)) + geom_density(col="white", fill="mediumturquoise", alpha=0.8) + ggtitle("Density Plot of Weight (kg)")
grid.arrange(density_hp, density_speed, density_attack, density_defense, density_height, density_weight, ncol=2)


# Bar Graph :

library(gridExtra)
type_1_poke <- ggplot(data=pokemon, aes(type1)) + geom_bar(aes(fill=..count..), alpha=0.8) + theme(axis.text.x = element_text(angle = 90, hjust = 0)) + ggtitle("Distribution Based on Type-1") + coord_flip()
type_2_poke <- ggplot(data=pokemon, aes(type2)) + geom_bar(aes(fill=..count..), alpha=0.8) + theme(axis.text.x = element_text(angle = 90, hjust = 0)) + ggtitle("Distribution Based on Type-2") + coord_flip()
grid.arrange(type_1_poke, type_2_poke, ncol=2)

# Scatter Plot :
ggplot(data=pokemon, aes(attack, defense))  +
geom_point(aes(color=is_legendary), alpha=0.8) +
scale_color_gradient(low="darkblue", high="red") + ggtitle("Defense vs Attack Characterstics") +
geom_label(data = subset(pokemon,attack > 150 | defense > 150 | attack < 25), aes(label=name),boxplot.matrix = 0.35, points.default = 0.5, segment ='grey50')

# Heatmap :
library(RColorBrewer)
library(maps)
library(melt)
hmap_attr <- select(pokemon, type1, is_legendary, hp, defense, attack, sp_attack, sp_defense, speed)
hmap_attr_leg <- filter(hmap_attr, is_legendary == 1)
hmap_attr_nor <- filter(hmap_attr, is_legendary == 0)
hmap_attr_leg <- group_by(hmap_attr_leg, type1)
hmap_attr_nor <- group_by(hmap_attr_nor, type1)
hmap_attr_leg <- summarise(hmap_attr_leg, hp=median(hp), attack=median(attack), defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense), speed=median(speed))
hmap_attr_leg_m <- melt(hmap_attr_leg)
hmap_attr_nor <- summarise(hmap_attr_nor, hp=median(hp), attack=median(attack), defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense), speed=median(speed))
hmap_attr_nor_m <- melt(hmap_attr_nor)
hm.palette <- colorRampPalette(rev(brewer.pal(5, 'RdYlBu')), space='Lab')
ggplot(data=hmap_attr_leg_m, aes(type1, variable)) + geom_tile(aes(fill=value)) + ggtitle("Legendary Pokemon: Type1 - Attribute") + scale_fill_gradientn(colours = hm.palette(100)) + theme(axis.text.x = element_text(angle=90, hjust=0)) + coord_equal()

hm.palette <- colorRampPalette(rev(brewer.pal(5, 'RdYlBu')), space='Lab')
ggplot(data=hmap_attr_nor_m, aes(type1, variable)) + geom_tile(aes(fill=value)) + ggtitle("Non-Legendary Pokemon: Type1 - Attribute") + scale_fill_gradientn(colours = hm.palette(100)) + theme(axis.text.x = element_text(angle=90, hjust=0)) + coord_equal()


# Corelation Matrix :
library(RColorBrewer)
library(ggrepel)
library(melt)
hmap_attr <- select(pokemon, type1, is_legendary, hp, defense, attack, sp_attack, sp_defense, speed)
hmap_attr_leg <- filter(hmap_attr, is_legendary == 1)
hmap_attr_nor <- filter(hmap_attr, is_legendary == 0)
hmap_attr_leg <- group_by(hmap_attr_leg, type1)
hmap_attr_nor <- group_by(hmap_attr_nor, type1)
hmap_attr_leg <- summarise(hmap_attr_leg, hp=median(hp), attack=median(attack), defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense), speed=median(speed))
hmap_attr_nor <- summarise(hmap_attr_nor, hp=median(hp), attack=median(attack), defense=median(defense), sp_attack=median(sp_attack), sp_defense=median(sp_defense), speed=median(speed))
row.names(hmap_attr_leg) <- hmap_attr_leg$type1
hmap_attr_leg$type1 <- NULL
hmap_attr_leg$is_legendary <- NULL
row.names(hmap_attr_nor) <- hmap_attr_nor$type1
hmap_attr_nor$type1 <- NULL
hmap_attr_nor$is_legendary <- NULL
hmap_attr_leg_cor <- cor(hmap_attr_leg)
hmap_attr_leg_cor_m <- melt(hmap_attr_leg_cor)
hm.palette <- colorRampPalette(rev(brewer.pal(5, 'GnBu')), space='Lab')
ggplot(data=hmap_attr_leg_cor_m, aes(Var1, Var2)) + geom_tile(aes(fill=value)) + ggtitle("Attribute Correlation - Legendary") + scale_fill_gradientn(colours = hm.palette(100)) + coord_equal()


# Box Plot:
library(dplyr)
library(gridExtra)

boxplot <- select(pokemon, type1, is_legendary, hp, defense, attack, sp_attack, sp_defense, speed)
boxplot <- filter(boxplot, is_legendary == 1)
boxplot <- filter(boxplot, is_legendary == 0)
boxplot <- gather(boxplot, value, -c(type1, is_legendary))
boxplot <- gather(boxplot, value, -c(type1, is_legendary))
bp_leg <- ggplot(data = boxplot, aes(attribute, value)) + geom_boxplot(fill="green4") + ggtitle("legendary pokemon")
bp_nor <- ggplot(data = boxplot, aes(attribute, value)) + geom_boxplot(fill = "yellow2") + ggtitle("normal pokemon")
grid.arrange(bp_leg, bp_nor,ncol=2)

# Bubble Chart :
library(ggplot2)
library(dplyr)
library(gapminder)
pokemon<- gapminder %>% filter(pokemon=="sp_attack") %>% dplyr::select(-pokemon)
ggplot(pokemon, aes(x=sp_attack, y=total, size = 0.5)) +
  geom_point(alpha=0.7)

#Violin Plot :
library(ggplot2)
vp_leg <- ggplot(data = box_plot_attr_leg_long, aes(attribute, value)) + geom_violin(fill="green4") + ggtitle("Legendary Pokemons")
vp_nor <- ggplot(data = box_plot_attr_nor_long, aes(attribute, value)) + geom_violin(fill="yellow2") + ggtitle("Normal Pokemons")
grid.arrange(vp_leg, vp_nor,ncol=2) 

# Hexbin Plot :
install.packages("hexbin")
library(hexbin)
set.seed(153)
x <- hp(1000)
y <- grass(1000)
bin <- hexbin(x,y)
plot(bin)

#Color Customization :
library(hexbin)
library(RColorBrewer)
set.seed(153)
x <- hp(1000)
y <- grass(1000)
bin <- hexbin(x,y)
plot(bin)
plot(bin, main="", colorRamp(colorRampPalette(c("Grren","Red",)),legend = F))

# Pie Chart
options(repr.plot.width=16, repr.plot.height=8)

pokemon %>% 
  filter(Type.1 %in% c("Water", "Normal", "Grass", "Bug", "Psychic", "Fire")) %>% 
  group_by(Type.1) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  
  ggplot(aes(x = "", y = n)) +
  geom_bar(stat = "identity", aes(fill = as.factor(Type.1)), position = "stack") +
  coord_polar("y", start=0) +
  
  geom_label(aes(y = n/2.5 + c(0, cumsum(n)[-length(n)]), label = formatC(n, format="f", big.mark=",", digits=0)), size=5) +
  scale_fill_manual(values = colorsRainbow) +
  new_theme + theme(axis.text = element_blank(), axis.title = element_blank(),
                    legend.title = element_text(face = "bold", size = 15),
                    legend.text = element_text(size=13)) +
  labs(title = "Pie Chart", subtitle = "Pokemons", fill = "Pokemon Type")



