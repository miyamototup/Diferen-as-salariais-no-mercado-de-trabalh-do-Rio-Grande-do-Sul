#X= é a listagem de tabelas contidas no BD Sqlite que serão utilizadas
#y é o nome da conexão com o banco SQLite
emp_form<- function (x,y){
  l_emprego_formal=list()
  
  for (i in 1:length(x)){
  k=x[[i]]
  emprego_formal=dbGetQuery(y,paste("select `Município` from",k))
  emprego_formal=as.data.frame(table(emprego_formal$Município))
  emprego_formal$Ano=1999+i
  colnames(emprego_formal)=c('Codigo','Empregos Formais','Ano')
  l_emprego_formal[[i]]=emprego_formal
  }
  emprego_formal=do.call(rbind.data.frame, l_emprego_formal)
  return(emprego_formal)
}


#Taxa de crescimento linear das variáveis da RAIS
#objeto x deve ser um data frame na ordem Codigo, variável Ano
tx_cresc <- function(x){
  tx_crescimento=x
  colnames(tx_crescimento)=c('Codigo','Variavel','Ano')
  tx_crescimento$logtx=log(tx_crescimento$Variavel)
  tx_crescimento=tx_crescimento[,c(1,3,4)]
  
  tx_crescimento2=list()
  tx_crescimento=split(tx_crescimento,paste(tx_crescimento$Codigo))
  
  for (i in 1:length(tx_crescimento)){
    cept=lm(tx_crescimento[[i]]$logtx~tx_crescimento[[i]]$Ano,data= tx_crescimento[[i]])
    slope=as.data.frame((exp(cept$coefficients[2]))-1)
    cept2=as.data.frame(cbind(as.character(tx_crescimento[[i]]$Codigo[1]),slope))
    tx_crescimento2[[i]]=cept2
  }
  
  tx_crescimento=do.call(rbind.data.frame, tx_crescimento2)
  colnames(tx_crescimento)=c('Codigo','TXC')
  return(tx_crescimento)
}


##Cácula a renda média nominal anual dos municípios 
#x é o vetor UF e y a conexão com o SQLite
rend_med_nom<-function(x,y){
    lrenda=list()
  for (i in 1:length(x)){
    k=uf[[i]]
    renda=dbGetQuery(y,paste("select `Município`,`Vl.Remun.Dezembro.Nom` from",k))
    #Excluiu-se os indíduos com valor de renda igual a zero
    renda=renda[renda$Vl.Remun.Dezembro.Nom!=0,]
    numocupmun=as.data.frame(table(renda$Município))
    rendamun=aggregate(renda[,c(2)],by=list(Muncipio=renda$Município),sum,na.rm=T)
   renda=merge(numocupmun,rendamun,by.x=c('Var1'),by.y=c('Muncipio'))
    colnames(renda)=c('Codigo','NumPost','Rendmun')
    #Criando variável renda média por trabalhador ocupado
    renda$rendmedtrab=renda$Rendmun/renda$NumPost
    renda$Ano=1999+i
    lrenda[[i]]=renda
  }
    renda=do.call(rbind.data.frame, lrenda)
    return(renda)
}

#Propoção de analfabetos e de ocupados com ensino superior
#Observação
#Até 2005 = `Grau.Instrução.2005.1985`
#Depois (i=7) =Escolaridade.após.2005
escolaridade<-function(x,y){
uf=x
rs=y
lescol=list()

#Até 2005
for (i in 1:6){
  k=uf[[i]]
  escol=dbGetQuery(rs,paste("select `Município`,`Grau.Instrução.2005.1985` from",k))
  escol=as.data.frame(table(escol$Município,escol$Grau.Instrução.2005.1985))
  r=aggregate(escol[,c(3)],by=list(Municipio=escol$Var1),sum,na.rm=T)
  r1=escol[escol$Var2==1,]
  r2=escol[escol$Var2==9,]
  escol=merge(r,r1,by.x=c('Municipio'),by.y=c('Var1'))
  escol=merge(escol,r2,by.x=c('Municipio'),by.y=c('Var1'))
  colnames(escol)=c('Municipio','TotalEduc','Analf','TotalAnalf','Superior','TotalSuper')
  escol$Percanalf=escol$TotalAnalf/escol$TotalEduc
  escol$Percsuper=escol$TotalSuper/escol$TotalEduc
  escol$Ano=1999+i
  lescol[[i]]=escol
}  
#Pós 2005
for (i in 7:length(uf)){
  k=uf[[i]]
  escol=dbGetQuery(rs,paste("select `Município`,`Escolaridade.após.2005` from",k))
  escol=as.data.frame(table(escol$Município,escol$Escolaridade.após.2005))
  r=aggregate(escol[,c(3)],by=list(Municipio=escol$Var1),sum,na.rm=T)
  r1=escol[escol$Var2==1,]
  r2=escol[escol$Var2==9,]
  escol=merge(r,r1,by.x=c('Municipio'),by.y=c('Var1'))
  escol=merge(escol,r2,by.x=c('Municipio'),by.y=c('Var1'))
  colnames(escol)=c('Municipio','TotalEduc','Analf','TotalAnalf','Superior','TotalSuper')
  escol$Percanalf=escol$TotalAnalf/escol$TotalEduc
  escol$Percsuper=escol$TotalSuper/escol$TotalEduc
  escol$Ano=1999+i
  lescol[[i]]=escol
  
}  


escol=do.call(rbind.data.frame,lescol)
escol=escol[,c(1,2,4,6:9)]
return(escol)

}
