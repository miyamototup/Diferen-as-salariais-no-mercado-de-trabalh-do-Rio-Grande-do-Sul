library(DBI)
library(RSQLite)

#Criando o banco de dados da RAIS em formato SQL
setwd("/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/BDSQL")
rs=dbConnect(SQLite(),dbname="RAISRS")

#Definindo a localização dos arquivos txt que serão convertidos em SQL
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
#Capta nome dos arquivos para fazer o looping de criação do BD
em=list.files(pattern = "*.txt")

for (i in 1:length(em)){
tb=read.csv(em[[i]], skip=0, sep=";",dec=",",header=T, fileEncoding="Windows-1252")
nome=paste("RAIS",1999+i, sep="")
dbWriteTable(conn=rs,name=nome,value=tb,append=F)
}


##############################
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source("rais.R")
library(RSQLite)
library(ggplot2)
library(maptools)
library(maps)


setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/BDSQL')
rs=dbConnect(SQLite(),dbname="RAISRS")

#Checando tabelas
dbListTables(rs)

uf=c("RAIS2000","RAIS2001","RAIS2002","RAIS2003","RAIS2004","RAIS2005","RAIS2006",
     "RAIS2007","RAIS2008","RAIS2009","RAIS2010","RAIS2011","RAIS2012","RAIS2013",
     "RAIS2014","RAIS2015","RAIS2016","RAIS2017")

#Quantidade de empregos formais no RS ao longo dos anos (Função de rais.R)
#Passo como argumentos a lista de tabelas da RAIS e o nome da conexão com o Banco de Dados
emp_form_rs=emp_form(uf,rs)

#Taxa de crescimento do emprego formal no RS (Função rais.R)
#Passo como argumento a tabela emp_form_rs obeitda anteriormente.
tx_emp_form_rs=tx_cresc(emp_form_rs)

##########################
###Exposição de resultados
#Evolução da Quantidade de Empregos formais no RS
emp_tot=aggregate(emp_form_rs[,c(2)],by=list(Ano=emp_form_rs$Ano),sum,na.rm=T)
colnames(emp_tot)[2]="Q.Empregos Formais"
emp_tot$QEmp2=emp_tot$`Q.Empregos Formais`/1000000

#ggplot2 - #Evolução da Quantidade de Empregos formais no RS
p = ggplot(data=emp_tot, aes(y=emp_tot$QEmp2, x=emp_tot$Ano))
p0 <- p + geom_line(size=0.9) + labs(y='', x='') + theme(axis.text.y=element_text(size=15)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1,size=15),panel.grid.major.x=element_blank()) +
  scale_y_continuous(limits=c(0,max(emp_tot[3])), breaks = seq(0, max(emp_tot[,3]+(emp_tot[,3]*125)), by=1)) +
  scale_x_continuous(limits=c(min(emp_tot[,1]), max(emp_tot[,1])), breaks = seq(min(emp_tot[,1]),
  max(emp_tot[,1]), by=2)) 

#Salva automaticamente em pdf no working directory
#pdf(file="Quantidade de empregos formais no RS",width=7,height=4.5)
#p0
#dev.off()



######################
####################
#Mapas
#malha de municípios
municipios=readShapeSpatial('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/Shapes/municipio5564/municipio5564.shp')
#malha de estados
estados=readShapeSpatial('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/Shapes/estados_2010/estados_2010.shp')
#malha de microrregiões
microrregioes=readShapeSpatial('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/Shapes/microregiao/microregiao.shp')

#Selecionar apenas RS em termos de municípios, UF e microrregioes
municipios@data$ordem=1:dim(municipios@data)[1]
rsmun=municipios[municipios@data$NOME_UF=='RS',]

estados@data$ordem=1:dim(estados@data)[1]
rsest=estados[estados@data$sigla=='RS',]

microrregioes@data$ordem=1:dim(microrregioes@data)[1]
rsmicro=microrregioes[microrregioes@data$NOME_UF=='RS',]




#################################### Taxa de crescimento do emprego formal nas microrregiões
#Agrupando a quantidad de empregos formais por microrregião
emp_form_rs_micr=merge(emp_form_rs,rsmun@data,by.x='Codigo',by.y='CODIGO_MUN',all.x=T,all.y=T)
emp_form_rs_micr=aggregate(emp_form_rs_micr[,c(2)],by=list(Codmicro=emp_form_rs_micr$CODIGO_MIC,
    Ano=emp_form_rs_micr$Ano),sum, na.rm=T)
emp_form_rs_micr$Codmicro=as.numeric(levels(emp_form_rs_micr$Codmicro)[emp_form_rs_micr$Codmicro])


#taxa de crescimento do emprego formal nas microregiões do RS
emp_form_rs_micr=emp_form_rs_micr[,c(1,3,2)]
colnames(emp_form_rs_micr)=c('Codigo','Empregos Formais','Ano')
tx_emp_form_rs_micro=tx_cresc(emp_form_rs_micr)



##Cores originais
#rsmicro@data$cor[rsmicro@data$TXC<=0.03065528]='powderblue'
#rsmicro@data$cor[rsmicro@data$TXC>0.03065528&rsmicro@data$TXC<=0.03634532]='steelblue1'
#rsmicro@data$cor[rsmicro@data$TXC>0.03634532&rsmicro@data$TXC<=0.04586250]='steelblue3'
#rsmicro@data$cor[rsmicro@data$TXC>0.04586250]='royalblue4'

#Mapa
#0%        25%        50%        75%       100% 
#0.01912161 0.03065528 0.03634532 0.04586250 0.05465467 
rsmicro@data$ordem=1:dim(rsmicro@data)[1]
rsmicro$COD_MICROR=as.numeric(levels(rsmicro$COD_MICROR)[rsmicro$COD_MICROR])
rsmicro@data=merge(rsmicro@data,tx_emp_form_rs_micro,by.x='COD_MICROR',by.y='Codigo',all.x=T,all.y=T)
rsmicro@data$cor[rsmicro@data$TXC<=0.03065528]='gray88'
rsmicro@data$cor[rsmicro@data$TXC>0.03065528&rsmicro@data$TXC<=0.03634532]='gray60'
rsmicro@data$cor[rsmicro@data$TXC>0.03634532&rsmicro@data$TXC<=0.04586250]='gray36'
rsmicro@data$cor[rsmicro@data$TXC>0.04586250]='gray19'
plot(rsmicro, border='gray0', lwd=1.2, axes=,col=rsmicro@data$cor)
legenda=as.character(c("2% a 3%","3% a 3,5%","3,5% a 4,5% ","4,5% a 5,5%"))
#legenda=as.character(c("1","2","3","4"))
cores=as.character(c("gray88","gray60","gray36","gray19"))
legend(x=-58.6,y=-30.8, legenda, fill=cores, bty="n",cex=1.5)
#legend(x=-57.1,y=-30.8, legenda, fill=cores, bty="n",cex=1)
map.scale(x=-52.2, y=-33.1,relwidth=0.09,metric=T,ratio=F,cex=0.75)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source(compassRose(-51.37,-32.5))





########################Taxa de crescimento da renda

#Cácula a renda média nominal anual dos municípios passando a lista de tabelas da RAIS
# e o nome da conexão com o banco de dados (Função de rais.R)
rendanom=rend_med_nom(uf,rs)


#Inflação
#Dividir por indivíduo e trazer os valores para 2016 ou 2015 corrigindo pela inflação
#Tratamento do IPCA
ipca=read.csv("/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/IPCA/ipca.csv",header=T)

ipca=ipca[ipca$Ano>=2000&ipca$Ano<=2018,]
ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 2000. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}

#Valores reais de 2018. Multiplico o valor monetário por esse número
ipca[(dim(ipca)[1]),]=NA
ipca$V3[(dim(ipca)[1])]=1
ipca$V3[(dim(ipca)[1])-1]=ipca$V1[(dim(ipca)[1])-1]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}

ipca=ipca[,c(1,5)]

#Renda média real dos municípios ano a ano
rendareal=merge(rendanom,ipca,by=c('Ano'),all.x=T,all.y=F)
rendareal$rendmedtrabreal=rendareal$rendmedtrab*rendareal$V3


###Já que tenho a renda média real por município, acho que posso agregar e criar um gráfico
#igual fiz para o número de postos de trabalho
##########################
###Exposição de resultados
#Evolução do salário real no RS
salreal=aggregate(rendareal[,c(7)],by=list(Ano=rendareal$Ano),mean,na.rm=T)
colnames(salreal)[2]='salario'

#ggplot2 - #Evolução do salário real no RS
p = ggplot(data=salreal, aes(y=salreal$salario, x=salreal$Ano))
p0 <- p + geom_line(size=0.9) + labs(y='', x='') + theme(axis.text.y=element_text(size=15)) +
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1,size=15),panel.grid.major.x=element_blank()) +
  scale_y_continuous(limits=c(0,max(salreal[2])), breaks = seq(0, max(salreal[,2]+(salreal[,2]*1.25)), by=400)) +
  scale_x_continuous(limits=c(min(salreal[,1]), max(salreal[,1])), breaks = seq(min(salreal[,1]),
                                                                                max(salreal[,1]), by=2)) 

#Salva automaticamente em pdf no working directory
#pdf(file="Evolução do salário médio real no RS",width=7,height=4.5)
#p0
#dev.off()


######Taxa de crescimento da renda real nas microrregiões do RS
rendareal_rs_micr=merge(rendareal,rsmun@data,by.x='Codigo',by.y='CODIGO_MUN',all.x=T,all.y=T)
rendareal_rs_micr=aggregate(rendareal_rs_micr[,c(5)],by=list(Codmicro=rendareal_rs_micr$CODIGO_MIC,
                                                           Ano=rendareal_rs_micr$Ano),mean, na.rm=T)
rendareal_rs_micr$Codmicro=as.numeric(levels(rendareal_rs_micr$Codmicro)[rendareal_rs_micr$Codmicro])
rendareal_rs_micr=rendareal_rs_micr[,c(1,3,2)]
tx_rendareal=tx_cresc(rendareal_rs_micr)


#Mapa
#0%        25%        50%        75%       100% 
#0.07671502 0.09064497 0.09272684 0.09495794 0.10187307 
rsmicro=microrregioes[microrregioes@data$NOME_UF=='RS',]
rsmicro@data$ordem=1:dim(rsmicro@data)[1]
rsmicro$COD_MICROR=as.numeric(levels(rsmicro$COD_MICROR)[rsmicro$COD_MICROR])
rsmicro@data=merge(rsmicro@data,tx_rendareal,by.x='COD_MICROR',by.y='Codigo',all.x=T,all.y=T)
rsmicro@data$cor[rsmicro@data$TXC<=0.09064497]='gray88'
rsmicro@data$cor[rsmicro@data$TXC>0.09064497&rsmicro@data$TXC<=0.09272684]='gray60'
rsmicro@data$cor[rsmicro@data$TXC>0.09272684&rsmicro@data$TXC<=0.09495794]='gray36'
rsmicro@data$cor[rsmicro@data$TXC> 0.09495794]='gray19'
plot(rsmicro, border='gray0', lwd=1.2, axes=,col=rsmicro@data$cor)
legenda=as.character(c("7,5% a 9%","9% a 9,3%","9,3% a 9,5% ","9,5% a 10,2%"))
#legenda=as.character(c("1","2","3","4"))
cores=as.character(c("gray88","gray60","gray36","gray19"))
legend(x=-58.6,y=-30.8, legenda, fill=cores, bty="n",cex=1.5)
#legend(x=-57.1,y=-30.8, legenda, fill=cores, bty="n",cex=1)
map.scale(x=-52.2, y=-33.1,relwidth=0.09,metric=T,ratio=F,cex=0.75)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source(compassRose(-51.37,-32.5))



##########################
#########################
##########################
#Mapas sobre participação de ocupados com ensino superior em relação aos ocupados totais por município

#Proporção de analfabetos e ocupados com ensino superior nos municípios
p_escolaridade=escolaridade(uf,rs)

#Agora preciso calcular a proporção por microrregião para fazer os mapas
p_escol_micr=merge(p_escolaridade,rsmun@data,by.x='Municipio',by.y='CODIGO_MUN',all.x=T,all.y=T)
p_escol_micr=aggregate(p_escol_micr[,c(2:4)],by=list(Codmicro=p_escol_micr$CODIGO_MIC,
                                                   Ano=p_escol_micr$Ano),sum, na.rm=T)
p_escol_micr$Percanalf=p_escol_micr$TotalAnalf/p_escol_micr$TotalEduc
p_escol_micr$Percsuper=p_escol_micr$TotalSuper/p_escol_micr$TotalEduc


#agora só isolar, percentual de analfabetos em 2000 e 2017 e taxa de crescumento linear no períod
#percentual de ocupados com ensino superior em 2000 e 2017 e taxa de crescimento linear no período
p_escol_2000=p_escol_micr[p_escol_micr$Ano==2000,]
p_escol_2017=p_escol_micr[p_escol_micr$Ano==2017,]

#####################
#Analfabetos 2000 e ocupados com ensino superior em 2000
rsmicro=microrregioes[microrregioes@data$NOME_UF=='RS',]
rsmicro@data$ordem=1:dim(rsmicro@data)[1]
rsmicro$COD_MICROR=as.numeric(levels(rsmicro$COD_MICROR)[rsmicro$COD_MICROR])
rsmicro@data=merge(rsmicro@data,p_escol_2000,by.x='COD_MICROR',by.y='Codmicro',all.x=T,all.y=T)

#Analfabetos 2000
#         0%         25%         50%         75%        100% 
#0.004773507 0.007679511 0.011495674 0.018382844 0.033137513 
rsmicro@data$cor[rsmicro@data$Percanalf<=0.007679511]='gray88'
rsmicro@data$cor[rsmicro@data$Percanalf>0.007679511&rsmicro@data$Percanalf<=0.011495674]='gray60'
rsmicro@data$cor[rsmicro@data$Percanalf>0.011495674&rsmicro@data$Percanalf<=0.018382844]='gray36'
rsmicro@data$cor[rsmicro@data$Percanalf>0.018382844]='gray19'
plot(rsmicro, border='gray0', lwd=1.2, axes=,col=rsmicro@data$cor)
legenda=as.character(c("0,4% a 0,7%","0,7% a 1,1%","1,1% a 1,8% ","1,8% a 3,5%"))
#legenda=as.character(c("1","2","3","4"))
cores=as.character(c("gray88","gray60","gray36","gray19"))
legend(x=-58.6,y=-30.8, legenda, fill=cores, bty="n",cex=1.5)
#legend(x=-57.1,y=-30.8, legenda, fill=cores, bty="n",cex=1)
map.scale(x=-52.2, y=-33.1,relwidth=0.09,metric=T,ratio=F,cex=0.75)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source(compassRose(-51.37,-32.5))






#Superior 2000
#         0%        25%        50%        75%       100% 
#0.02227802 0.05527770 0.07443279 0.08531233 0.12737782 
rsmicro@data$cor[rsmicro@data$Percsuper<=0.05527770]='gray88'
rsmicro@data$cor[rsmicro@data$Percsuper>0.05527770&rsmicro@data$Percsuper<=0.07443279]='gray60'
rsmicro@data$cor[rsmicro@data$Percsuper>0.07443279&rsmicro@data$Percsuper<=0.08531233]='gray36'
rsmicro@data$cor[rsmicro@data$Percsuper>0.08531233]='gray19'
plot(rsmicro, border='gray0', lwd=1.2, axes=,col=rsmicro@data$cor)
legenda=as.character(c("2,2% a 5,5%","5,5% a 7,5%","7,5% a 8,5% ","8,5% a 12,8%"))
#legenda=as.character(c("1","2","3","4"))
cores=as.character(c("gray88","gray60","gray36","gray19"))
legend(x=-58.6,y=-30.8, legenda, fill=cores, bty="n",cex=1.5)
#legend(x=-57.1,y=-30.8, legenda, fill=cores, bty="n",cex=1)
map.scale(x=-52.2, y=-33.1,relwidth=0.09,metric=T,ratio=F,cex=0.75)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source(compassRose(-51.37,-32.5))


#####################
#Analfabetos 2017 e ocupados com ensino superior em 2017
rsmicro=microrregioes[microrregioes@data$NOME_UF=='RS',]
rsmicro@data$ordem=1:dim(rsmicro@data)[1]
rsmicro$COD_MICROR=as.numeric(levels(rsmicro$COD_MICROR)[rsmicro$COD_MICROR])
rsmicro@data=merge(rsmicro@data,p_escol_2017,by.x='COD_MICROR',by.y='Codmicro',all.x=T,all.y=T)

#Analfabetos 2017
#0%          25%          50%          75%         100% 
#0.0008248103 0.0015138676 0.0025481602 0.0034057294 0.0070793504
rsmicro@data$cor[rsmicro@data$Percanalf<=0.0015138676]='gray88'
rsmicro@data$cor[rsmicro@data$Percanalf>0.0015138676&rsmicro@data$Percanalf<=0.0025481602]='gray60'
rsmicro@data$cor[rsmicro@data$Percanalf>0.0025481602&rsmicro@data$Percanalf<=0.0034057294]='gray36'
rsmicro@data$cor[rsmicro@data$Percanalf>0.0034057294]='gray19'
plot(rsmicro, border='gray0', lwd=1.2, axes=,col=rsmicro@data$cor)
legenda=as.character(c("0% a 0,15%","0,15% a 0,25%","0,25% a 0,35% ","0,35% a 0,70%"))
#legenda=as.character(c("1","2","3","4"))
cores=as.character(c("gray88","gray60","gray36","gray19"))
legend(x=-58.6,y=-30.8, legenda, fill=cores, bty="n",cex=1.5)
#legend(x=-57.1,y=-30.8, legenda, fill=cores, bty="n",cex=1)
map.scale(x=-52.2, y=-33.1,relwidth=0.09,metric=T,ratio=F,cex=0.75)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source(compassRose(-51.37,-32.5))


#Superior 2017
#0%       25%       50%       75%      100% 
#0.0658598 0.1296082 0.1438412 0.1555118 0.2074078
rsmicro@data$cor[rsmicro@data$Percsuper<=0.1296082]='gray88'
rsmicro@data$cor[rsmicro@data$Percsuper>0.1296082&rsmicro@data$Percsuper<=0.1438412]='gray60'
rsmicro@data$cor[rsmicro@data$Percsuper>0.1438412&rsmicro@data$Percsuper<=0.1555118]='gray36'
rsmicro@data$cor[rsmicro@data$Percsuper>0.1555118]='gray19'
plot(rsmicro, border='gray0', lwd=1.2, axes=,col=rsmicro@data$cor)
legenda=as.character(c("6,5% a 13%","13% a 14,5%","14,5% a 15,5% ","15,5% a 21,8%"))
#legenda=as.character(c("1","2","3","4"))
cores=as.character(c("gray88","gray60","gray36","gray19"))
legend(x=-58.6,y=-30.8, legenda, fill=cores, bty="n",cex=1.5)
#legend(x=-57.1,y=-30.8, legenda, fill=cores, bty="n",cex=1)
map.scale(x=-52.2, y=-33.1,relwidth=0.09,metric=T,ratio=F,cex=0.75)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source(compassRose(-51.37,-32.5))

#Calcular apenas a proporção de analfabetos e com ensino superior no inicio e no fim da 
#série ou, incluir também a taxa de crescimento da proporção da população analfabeta e
# com ensino superior.
#VOu calcular as taxas tb
tx_analf=tx_cresc(p_escol_micr[,c(1,6,2)])
colnames(tx_analf)[2]='tx_analf'
tx_analf$Codigo=as.numeric(levels(tx_analf$Codigo)[tx_analf$Codigo])

tx_super=tx_cresc(p_escol_micr[,c(1,7,2)])
colnames(tx_super)[2]='tx_super'
tx_super$Codigo=as.numeric(levels(tx_super$Codigo)[tx_super$Codigo])

#Mapas das taxas de crescimento de analfabetos e ocupados com ensino superior
tx_super_analf=merge(tx_super,tx_analf,by='Codigo',all.x=T,all.y=T)
rsmicro=microrregioes[microrregioes@data$NOME_UF=='RS',]
rsmicro@data$ordem=1:dim(rsmicro@data)[1]
rsmicro$COD_MICROR=as.numeric(levels(rsmicro$COD_MICROR)[rsmicro$COD_MICROR])
rsmicro@data=merge(rsmicro@data,tx_super_analf,by.x='COD_MICROR',by.y='Codigo',all.x=T,all.y=T)


#Taxa de crescimento dos analfabetos
#0%         25%         50%         75%        100% 
#-0.12970174 -0.10638672 -0.09454733 -0.07739341 -0.05400756
rsmicro@data$cor[rsmicro@data$tx_analf>=(-0.07739341)]='gray88'
rsmicro@data$cor[rsmicro@data$tx_analf<(-0.077393412)&rsmicro@data$tx_analf>=(-0.09454733)]='gray60'
rsmicro@data$cor[rsmicro@data$tx_analf<(-0.09454733)&rsmicro@data$tx_analf>=(-0.10638672)]='gray36'
rsmicro@data$cor[rsmicro@data$tx_analf<(-0.10638672)]='gray19'
plot(rsmicro, border='gray0', lwd=1.2, axes=,col=rsmicro@data$cor)
legenda=as.character(c("-5,5% a -7,7%","-7,7% a -9,5%","-9,5% a -10,6% ","-10,6% a -13%"))
#legenda=as.character(c("1","2","3","4"))
cores=as.character(c("gray88","gray60","gray36","gray19"))
legend(x=-58.6,y=-30.8, legenda, fill=cores, bty="n",cex=1.5)
#legend(x=-57.1,y=-30.8, legenda, fill=cores, bty="n",cex=1)
map.scale(x=-52.2, y=-33.1,relwidth=0.09,metric=T,ratio=F,cex=0.75)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source(compassRose(-51.37,-32.5))

#Taxa de crescimento dos ocupados com ensino superior
#0%         25%         50%         75%        100% 
#0.009987223 0.024525176 0.038003907 0.049084764 0.073639419 
rsmicro@data$cor[rsmicro@data$tx_super<=0.024525176]='gray88'
rsmicro@data$cor[rsmicro@data$tx_super>0.024525176&rsmicro@data$tx_super<=0.038003907]='gray60'
rsmicro@data$cor[rsmicro@data$tx_super>0.038003907&rsmicro@data$tx_super<=0.049084764]='gray36'
rsmicro@data$cor[rsmicro@data$tx_super>0.049084764]='gray19'
plot(rsmicro, border='gray0', lwd=1.2, axes=,col=rsmicro@data$cor)
legenda=as.character(c("0,9% a 2,5%","2,5% a 3,8%","3,8% a 5% ","5% a 7,4%"))
#legenda=as.character(c("1","2","3","4"))
cores=as.character(c("gray88","gray60","gray36","gray19"))
legend(x=-58.6,y=-30.8, legenda, fill=cores, bty="n",cex=1.5)
#legend(x=-57.1,y=-30.8, legenda, fill=cores, bty="n",cex=1)
map.scale(x=-52.2, y=-33.1,relwidth=0.09,metric=T,ratio=F,cex=0.75)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source(compassRose(-51.37,-32.5))

###########################################################
###########################################################
###Montar a base para as regressões
##############################
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ')
source("rais.R")
library(RSQLite)
library(maptools)


setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/BDSQL')
rs=dbConnect(SQLite(),dbname="RAISRS")

#Checando tabelas
dbListTables(rs)

uf=c("RAIS2000","RAIS2001","RAIS2002","RAIS2003","RAIS2004","RAIS2005","RAIS2006",
     "RAIS2007","RAIS2008","RAIS2009","RAIS2010","RAIS2011","RAIS2012","RAIS2013",
     "RAIS2014","RAIS2015","RAIS2016","RAIS2017")


#Em 2006 a variável Grau.Instrução.2005.1985 muda de nome para Escolaridade.após.2005
#de 2006 a 2017 Raça.Cor
#Tenho que garantir que isso não atrapalhe o cbind da lista criada
lpooled=list()
for (i in 1:length(uf)){
  k=uf[[i]]
  if(i<=6){
  rspooled=dbSendQuery(rs,paste("select `Município`, `Vl.Remun.Média.Nom`, 
`Faixa.Tempo.Emprego`,`Grau.Instrução.2005.1985`, `Idade`, `Sexo.Trabalhador` from",k))
  }
  else if (i>6){
  rspooled=dbSendQuery(rs,paste("select `Município`, `Vl.Remun.Média.Nom`, 
`Faixa.Tempo.Emprego`,`Escolaridade.após.2005`, `Idade`, `Sexo.Trabalhador`,
                                `Raça.Cor` from",k))
  }
  
  rspooled=fetch(rspooled,n=-1)
  rspooled$Ano=1999+i
  colnames(rspooled)[4]='Escolaridade'
  if(i<=6){
    rspooled$Raça.Cor=NA
  }
  
  lpooled[[i]]=rspooled
  
}

rspooled=do.call(rbind.data.frame,lpooled)
rm(lpooled) 
gc()


#Criar categorias e ajustar o BD
#PAdronizando as categorias
#Na categoria faixa.tempo.emprego vou exluir a categoria 99( não classificado),
#depois vou converter todas as categorias para factor, compatibilizá-las e criar as binárias
rspooled=rspooled[rspooled$Faixa.Tempo.Emprego!='99'|(!is.na(rspooled$Faixa.Tempo.Emprego)),]
rspooled$tempoemprego[rspooled$Faixa.Tempo.Emprego=='01'|rspooled$Faixa.Tempo.Emprego=='1'|
                        rspooled$Faixa.Tempo.Emprego=='02'|rspooled$Faixa.Tempo.Emprego=='2'|
                        rspooled$Faixa.Tempo.Emprego=='03'|rspooled$Faixa.Tempo.Emprego=='3']=1
rspooled$tempoemprego[rspooled$Faixa.Tempo.Emprego=='04'|rspooled$Faixa.Tempo.Emprego=='4'|
                        rspooled$Faixa.Tempo.Emprego=='05'|rspooled$Faixa.Tempo.Emprego=='5']=2
rspooled$tempoemprego[rspooled$Faixa.Tempo.Emprego=='06'|rspooled$Faixa.Tempo.Emprego=='6']=6
rspooled$tempoemprego[rspooled$Faixa.Tempo.Emprego=='07'|rspooled$Faixa.Tempo.Emprego=='7']=7
rspooled$tempoemprego[rspooled$Faixa.Tempo.Emprego=='08'|rspooled$Faixa.Tempo.Emprego=='8']=8


#Escolaridade
rspooled$tempoescola[rspooled$Escolaridade==1]=1
rspooled$tempoescola[rspooled$Escolaridade==2|rspooled$Escolaridade==3]=2
rspooled$tempoescola[rspooled$Escolaridade==4|rspooled$Escolaridade==5]=3
rspooled$tempoescola[rspooled$Escolaridade==6]=6
rspooled$tempoescola[rspooled$Escolaridade==7]=7
rspooled$tempoescola[rspooled$Escolaridade==8]=8
rspooled$tempoescola[rspooled$Escolaridade==9]=9
rspooled$tempoescola[rspooled$Escolaridade==10|rspooled$Escolaridade==11]=10


#Raça
#indigena 1, branca 2, preta 4, amarela 6, parda 8,não identificado 9
#Nesse caso não tem como juntar categorias. Vou deixar assim depois é só criar as binárias
rspooled$raca[rspooled$Raça.Cor=='01'|rspooled$Raça.Cor=='1']=1
rspooled$raca[rspooled$Raça.Cor=='02'|rspooled$Raça.Cor=='2']=2
rspooled$raca[rspooled$Raça.Cor=='04'|rspooled$Raça.Cor=='4']=4
rspooled$raca[rspooled$Raça.Cor=='06'|rspooled$Raça.Cor=='6']=6
rspooled$raca[rspooled$Raça.Cor=='08'|rspooled$Raça.Cor=='8']=8
rspooled$raca[rspooled$Raça.Cor=='09'|rspooled$Raça.Cor=='9']=9





#Mesorregiões
#malha de municípios
municipios=readShapeSpatial('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/Shapes/municipio5564/municipio5564.shp')
rsmun=municipios[municipios@data$NOME_UF=='RS',]
rsmun=rsmun@data[,c(9,7)]
rspooled=merge(rspooled,rsmun,by.x='Município',by.y='CODIGO_MUN',all.x=T,all.y=T)


#Preciso inserir o código das mesos e deixar os valores monetários em valores reais de 2017
#Inflação
ipca=read.csv("/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/IPCA/ipca.csv",header=T)

ipca=ipca[ipca$Ano>=2000&ipca$Ano<=2018,]
ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 2000. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}

#Valores reais de 2018. Multiplico o valor monetário por esse número
ipca[(dim(ipca)[1]),]=NA
ipca$V3[(dim(ipca)[1])]=1
ipca$V3[(dim(ipca)[1])-1]=ipca$V1[(dim(ipca)[1])-1]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}

ipca=ipca[,c(1,5)]
rspooled=merge(rspooled,ipca,by='Ano',all.x=T,all.y=T)
#Corrigindo os salários pela inflação
rspooled$remunmed=rspooled$Vl.Remun.Média.Nom*rspooled$V3

#salvar banco de dados no BDSQL. Logo antes de criar as binárias na unha já que model.matrix 
#não está rolando :/
#dbWriteTable(rs,name='BDREG1',value = rspooled)

#####################################################
library(RSQLite)
library(dummies)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/BDSQL/')
rs=dbConnect(SQLite(),dbname='RAISRS')

bd=dbGetQuery(rs,'Select *from BDREG1')

#binárias para anos
bd=cbind(bd,dummy(bd$Ano,sep='_ano_'))
#binárias para escolaridade
bd=cbind(bd,dummy(bd$tempoescola,sep='_tescol_'))
#binárias para sexo trabalhador
bd=cbind(bd,dummy(bd$Sexo.Trabalhador,sep='_sexo_'))
#binárias para emprego
bd=cbind(bd,dummy(bd$tempoemprego,sep='_temp'))
#binárias para raça
bd=cbind(bd,dummy(bd$raca,sep='_raca_'))
#binárias para mesorregião
bd=cbind(bd,dummy(bd$CODIGO_MES,sep='_meso_'))

#Criar variável idade ao quadrado
bd$idadequad=bd$Idade*bd$Idade

#Selecionar variáveis que serão utilizadas no modelo
bd=bd[,c(1,6,14,15:32,34:41,43,44,46:50,52:57,59:65,67)]
bd$logsal=log(bd$remunmed)

bd=bd[!is.infinite(bd$logsal),]
bd=bd[!is.na(bd$logsal),]

#dbWriteTable(rs,name='BDREG2',value = bd)


#Mesorregiões do RS
#COD_MESORE                     NOME_MESOR
#503       4301         Noroeste Rio-grandense
#516       4302         Nordeste Rio-grandense
#519       4303 Centro Ocidental Rio-grandense
#522       4304  Centro Oriental Rio-grandense
#525       4305  Metropolitana de Porto Alegre
#531       4306         Sudoeste Rio-grandense
#534       4307          Sudeste Rio-grandense



############################################
###Modelo de regressão linear com dados empilhados
library(RSQLite)
library(biglm)
setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/BDSQL/')
rs=dbConnect(SQLite(),dbname='RAISRS')

bd=dbGetQuery(rs,'Select *from BDREG2')

gc()

#A única função que permite utilizar toda a base de dados é a biglm. Infelizmente o artigo da RAIS utilizou
#apenas uma amostra da base. Acabei de descobrir :/.
#Vou dividir a base bd em 4 partes e ir adicionando, cada uma dessas partes de cada vez ao modelo

#A base tem essa quantidade de linhas/observações
dim(bd)[1]

#Portanto cada parte da base deve ter
dim(bd)[1]/4

#A regressão então deverá ser feita acrescentando cada um dos seguintes intervalos de cada vez

#Intervalor 1 
# 1 a dim(bd)[1]/4
bdmovel=bd[1:(dim(bd)[1]/4),]

gc()

linmod=biglm(logsal ~ Idade +  idadequad + bd_ano_2001 + bd_ano_2002 + bd_ano_2003 +
               bd_ano_2004 + bd_ano_2005 + bd_ano_2006 + bd_ano_2007 + bd_ano_2008 + bd_ano_2009 +
               bd_ano_2010 + bd_ano_2011 + bd_ano_2012 + bd_ano_2013 + bd_ano_2014 + bd_ano_2015 +
               bd_ano_2016 + bd_ano_2017 + bd_tescol_2 + bd_tescol_3 + bd_tescol_6 +
               bd_tescol_7 + bd_tescol_8 + bd_tescol_9 + bd_tescol_10 + bd_sexo_2 +
               bd_temp2 + bd_temp6 + bd_temp7 + bd_temp8 + bd_raca_2 + bd_meso_4302 + 
               bd_meso_4303 + bd_meso_4304 + bd_meso_4305 + bd_meso_4306 + bd_meso_4307, data=bdmovel)

gc()

#Agora vou adicionar a segunda parte do bd, ou seja de ((dim(bd)[1]/4)+1) até ((dim(bd)[1]/4)*2)
bdmovel=bd[((dim(bd)[1]/4)+1):((dim(bd)[1]/4)*2),]
gc()
linmod=update(linmod,bdmovel)
gc()

#Terceira parte do bd na atualização de (((dim(bd)[1]/4)*2)+1) a ((dim(bd)[1]/4)*3)
bdmovel=bd[(((dim(bd)[1]/4)*2)+1):((dim(bd)[1]/4)*3),]
gc()
linmod=update(linmod,bdmovel)
gc()

#Atualizando com a quarta e última parte do bd de (((dim(bd)[1]/4)*3)+1) a (dim(bd)[1])
bdmovel=bd[(((dim(bd)[1]/4)*3)+1):(dim(bd)[1]),]
gc()
linmod=update(linmod,bdmovel)
gc()

resultado=summary(linmod)

###Regressão linear para anos específicos
Ano=c(2000,2003,2006,2009,2012,2015,2017)
linmodlist=list()

for (i in 1:length(Ano)){
  bdmovel=bd[bd$Ano==Ano[[i]],]
  linmod=lm(logsal ~ Idade +  idadequad + bd_tescol_2 + bd_tescol_3 + bd_tescol_6 +
              bd_tescol_7 + bd_tescol_8 + bd_tescol_9 + bd_tescol_10 + bd_sexo_2 +
              bd_temp2 + bd_temp6 + bd_temp7 + bd_temp8 + bd_raca_2 + bd_meso_4302 + 
              bd_meso_4303 + bd_meso_4304 + bd_meso_4305 + bd_meso_4306 + bd_meso_4307, data=bdmovel)
  linmodlist[[i]]=linmod
}

#Summary
res2000=summary(linmodlist[[1]])
res2003=summary(linmodlist[[2]])
res2006=summary(linmodlist[[3]])
res2009=summary(linmodlist[[4]])
res2012=summary(linmodlist[[5]])
res2015=summary(linmodlist[[6]])
res2017=summary(linmodlist[[7]])


#######################################################################
#Modelos de regressão quantílica


###Tentativa de fazer para a base toda. Vamos ver se tem RAM para isso ou se  pacote permite.
#Não dá. Ram insuficiente. vou fazer apenas para os anos de interesse 2000,2006,2012 e 2017
library(RSQLite)
library(quantreg)

setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/BDSQL/')
rs=dbConnect(SQLite(),dbname='RAISRS')


Ano=c(2000,2003,2006,2009,2012,2015,2017)
#lquantreg=list()

for (i in 6:length(Ano)){
  bd=dbGetQuery(rs, paste("Select *from BDREG2 where `Ano`=",Ano[[i]]))
  
  #Atribuir os valores x e y da equação de regressão
  y=cbind(bd$logsal)
  
  if (Ano[[i]]<=2003){
    #2000 a 
    x=cbind(as.matrix(bd[,c(2,50,23:28,31,33:36,44:49)]))
  } else if (Ano[[i]]>2003){
    x=cbind(as.matrix(bd[,c(2,50,23:29,31,33:36,38,44:49)]))
  }
  
  gc()
  
  ##
  #caminho de salvamento dos backups individuais
  setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/Bakup-quantilica/')
  
  #Regressão para o primeiro quartil de renda
  q25bd=rq(y~x, data=bd,tau=0.25)
  pq=summary(q25bd)
  pq=pq$coefficients
  write.csv(pq,file=paste(Ano[[i]],'25',sep='_'),row.names=F)
  #lquantreg[[paste(Ano[[i]],'25',sep='_')]]=q25bd
  gc()
  
  #Regressão para o segundo quartil de renda
  q50bd=rq(y~x, data=bd,tau=0.50)
  sq=summary(q50bd)
  sq=sq$coefficients
  write.csv(sq, file=paste(Ano[[i]],'50',sep='_'),row.names=F)
  #lquantreg[[paste(Ano[[i]],'50',sep='_')]]=q50bd
  gc()
  
  #Regressão para o terceiro quartil de renda
  q75bd=rq(y~x, data=bd,tau=0.75)
  tq=summary(q75bd)
  tq=tq$coefficients
  write.csv(tq, file=paste(Ano[[i]],'75',sep='_'),row.names=F)
  #lquantreg[[paste(Ano[[i]],'75',sep='_')]]=q75bd
  gc()
  
}

#setwd('/Arquivos/Documentos/Pesquisa/MBA-ATAIZ/')
#    save.image(file='backuptemporario2.RData')

#load('backuptemporario2.RData')      

#save.image(file='backuptemporario3.RData')      