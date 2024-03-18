Observeretpriser<- 20:6*60
r_list<- 1:15/100   # inflationsrate yield curve - 
S_0_list<- (27:13)*105
T_list<- 1:15/20
K_list<- 32:46*45

r<-r_list[1]
S_0<- S_0_list
T<- T_list[1]
K<- K_list[1]


#her beskriver vi pris algoritmen for vores Heston model


Pværdier<- function(r,theta=c(kappa,hatv,sigma,rho,v_0),S_0,T,K){
  F<- S_0*exp(r*T) #Dette er vores forward price
  integrand1<- function(u) { #her udregner vi 2 forskellige integrander som vi så senere integrerer det store her er valget af Phi som kaldes vores characteristiske function
    z<- u-sqrt(as.complex(-1))
    xi<- theta[1]-theta[3]*theta[4]*sqrt(as.complex(-1))*z
    d<- sqrt(xi^2+theta[3]^2*(sqrt(as.complex(-1))*z+z^2))
    g1<- (xi+d)/(xi-d)
    phi<- (exp(sqrt(as.complex(-1))*z*(log(S_0)+r*T)+((theta[1]*theta[2])/theta[3]^2)*((xi+d)*T-2*log((1-g1*exp(d*T))/(1-g1)))+(theta[5]^2/theta[3]^2)*(xi+d)*(1-exp(d*T))/(1-g1*exp(d*T))))
    #  print(sqrt(as.complex(-1))*z*(log(S_0)+r*T))
    #  print(((theta[1]*theta[2])/theta[3]^2)*((xi+d)*T-2*log((1-g1*exp(d*T))/(1-g1))))
    #  print((theta[5]^2/theta[3]^2)*(xi+d)*(1-exp(d*T))/(1-g1*exp(d*T)))
    integral<-as.numeric(Re(exp((-sqrt(as.complex(-1))*u*log(K)))/(sqrt(as.complex(-1)*u))*phi)) #husk du har fjernet F
    return(integral)
  }
  integrand2<-function(u){
    xi<- theta[1]-theta[3]*theta[4]*sqrt(as.complex(-1))*u
    d<- sqrt(xi^2+theta[3]^2*(sqrt(as.complex(-1))*u+u^2))
    g1<- (xi+d)/(xi-d)
    phi<- (exp(sqrt(as.complex(-1))*u*(log(S_0)+r*T)+(theta[1]*theta[2]/theta[3]^2)*((xi+d)*T-2*log((1-g1*exp(d*T))/(1-g1) ))+(theta[5]^2/theta[3]^2)*(xi+d)*(1-exp(d*T))/(1-g1*exp(d*T))))
    integral<-as.numeric(Re(exp((-sqrt(as.complex(-1))*u*log(K)))/(sqrt(as.complex(-1)*u))*phi))
    #   print(sqrt(as.complex(-1))*u*(log(S_0)+r*T))
    #   print(((theta[1]*theta[2])/theta[3]^2)*((xi+d)*T-2*log((1-g1*exp(d*T))/(1-g1))))
    #   print((theta[5]^2/theta[3]^2)*(xi+d)*(1-exp(d*T))/(1-g1*exp(d*T)))
    return(integral)
  }     
  int1<- as.numeric(integrate(integrand1, lower = 0, upper = 100,subdivisions = 10000)[1])
  int2<- as.numeric(integrate(integrand2, lower = 0, upper = 100,subdivisions = 10000)[1])
  C<- 1/2*(S_0-exp(-r*T)*K)+exp(-r*T)/pi * (int1-K*int2) # her udregner vi vores pris ifølge modellen og de givne parametre
  return(C)
}



GradientHeston<- function(eps=c(1,2,3,4,5),r,theta=c(kappa,hatv,sigma,rho,v_0),S_0,T,K) { #her approksimerer vi gradienten af vores pris-funktion ved brug af small increment
  c_1=c(eps[1],0,0,0,0)
  c_2=c(0,eps[2],0,0,0)
  c_3=c(0,0,eps[3],0,0)
  c_4=c(0,0,0,eps[4],0)
  c_5=c(0,0,0,0,eps[5])
  h1=(Pværdier(r,theta+c_1,S_0,T,K)-Pværdier(r,theta-c_1,S_0,T,K))/(2*eps[1])
  h2=(Pværdier(r,theta+c_2,S_0,T,K)-Pværdier(r,theta-c_2,S_0,T,K))/(2*eps[2])
  h3=(Pværdier(r,theta+c_3,S_0,T,K)-Pværdier(r,theta-c_3,S_0,T,K))/(2*eps[3])
  h4=(Pværdier(r,theta+c_4,S_0,T,K)-Pværdier(r,theta-c_4,S_0,T,K))/(2*eps[4])
  h5=(Pværdier(r,theta+c_5,S_0,T,K)-Pværdier(r,theta-c_5,S_0,T,K))/(2*eps[5])
  return(c(h1,h2,h3,h4,h5))
}


Steepestdescent<- function(eps=c(0.001,0.001,0.001,0.001,0.001),r,theta,S_0=S_0_list,T=T_list,K=K_list){ #her laver vi vores calibrering 
  Pværdilist<- 1:length(K) # start med at give vores model estimat af priserne ud fra vores start gæt theta
  K_grad<-0.05
  for(i in 1:length(K)){
    Pværdilist[i]<-Pværdier(r[i],theta,S_0[i],T[i],K[i])
  }
  for(i in 1:length(K)){ #udregn herefter residualerne
    resid_list<-Pværdilist-Observeretpriser
  }
  J<-data.frame(kappa=double(), hatv=double(),sigma=double(),rho=double(),v_0=double(),
                stringsAsFactors=FALSE)
  for(i in 1:length(K)){
    J[nrow(J) + 1,] = GradientHeston(eps,r[i],theta,S_0[i],T[i],K[i]) # skal teknisk set være et minus her men det fikses senere
  } 
  J<- data.matrix(J)
  mu<- max(diag(J)) # https://arxiv.org/pdf/1511.08718.pdf sagde det var et godt valg why i do not know
  vstart<-2
  grad<- solve(t(J)%*%J+mu*diag(nrow=5))%*%t(J)%*%resid_list # vores søgning retnimng i følge https://en.wikipedia.org/wiki/Levenberg%E2%80%93Marquardt_algorithm
  #print(grad)
 for(i in 1:50){
   #print(mu) 
   resid_list2<- resid_list# vi starter med at gemme alle værdier fra skridt K
    grad2<- grad
    theta2<-theta
    J2<-J
    J<-5
    grad<- solve(t(J2)%*%J2+mu*diag(nrow=5))%*%t(J2)%*%resid_list2 #nu udregner vi alle værdier for K+1
    #print(solve(t(J2)%*%J2))
    theta<- theta-K_grad*grad # har sat 0.05 på da vi kan få uendelige eller ikke defineret priser hvis vi er for aggresive
   # print(theta)
  for(i in 1:length(K)){ # udregn nyeresidualer for det nye skridt
      resid_list[i]<-Pværdier(r[i],theta,S_0[i],T[i],K[i])-Observeretpriser[i]
   }
    delta_L<- t(grad)%*%(mu*grad+t(J2)%*%resid_list2)
    delta_F<- norm(resid_list2,type="2")-norm(resid_list,type="2") #tjek om det nye skridt er bedst
 #   print(norm(resid_list,type="2"))
  #  print(delta_F)
    #print(norm(resid_list,type="2"))
    print(norm(resid_list2,type="2"))
    if(delta_L>0 & delta_F>0) {
      mu=mu
      J<-data.frame(kappa=double(), hatv=double(),sigma=double(),rho=double(),v_0=double(),
                    stringsAsFactors=FALSE)
      for(i in 1:length(K)){
        J[nrow(J) + 1,] = GradientHeston(eps,r[i],theta2,S_0[i],T[i],K[i])
      } 
      J<- data.matrix(J)
      }
    else{
      mu<-mu*vstart
      vstart<- vstart*2
      theta<- theta2
      resid_list<- resid_list2
      grad<-grad2
      J<-J2
      }
 }
  return(theta)
}

GradientHeston(eps=c(0.0001,0.0001,0.0001,0.0001,0.0001),r_list[1],theta=c(3,0.05,0.3,-0.9,0.1),S_0_list[1],T_list[1],K_list[1])


Steepestdescent(eps=c(0.0001,0.0001,0.0001,0.0001,0.0001),r=r_list,theta=c(3,0.05,0.3,-0.9,0.1),S_0_list,T_list,K_list)


debug(Steepestdescent)

undebug(Steepestdescent)


#Validering

#starter med at genererer nogle samples
kappa_gentrue<- runif(n=100, min=0.5, max=5)
hatv_gentrue<-runif(n=100, min=0.05, max=0.95)
sigma_gentrue<-runif(n=100, min=0.05, max=0.95)
rho_gentrue<-runif(n=100, min=-0.9, max=-0.1)
v_0_gentrue<-runif(n=100, min=0.05, max=0.95)


for (i in 1:100){
print(Pværdier(0.01,theta=c(kappa_gentrue[i],hatv_gentrue[i],sigma_gentrue[i],rho_gentrue[i],v_0_gentrue[i]),2805,0.05,1400))
}

set.seed(12345)
kappa_gen<- runif(n=100, min=0.5, max=5)
hatv_gen<-runif(n=100, min=0.05, max=0.95)
sigma_gen<-runif(n=100, min=0.05, max=0.95)
rho_gen<-runif(n=100, min=-0.9, max=-0.1)
v_0_gen<-runif(n=100, min=0.05, max=0.95)


validvector<- data.frame(kappa=double(), hatv=double(),sigma=double(),rho=double(),v_0=double(),
                                stringsAsFactors=FALSE)

for(i in 1:100){
  Observeretpriser<- Pværdier(0.01,theta=c(kappa_gentrue[i],hatv_gentrue[i],sigma_gentrue[i],rho_gentrue[i],v_0_gentrue[i]),2805,0.25,1400)
  thetaQ<- c(kappa_gen[i],hatv_gen[i],sigma_gen[i],rho_gen[i],v_0_gen[i])
  validvector[i,]<-Steepestdescent(eps=c(0.0001,0.0001,0.0001,0.0001,0.0001),0.01,theta=thetaQ,2805,0.25,1400)-c(kappa_gentrue[i],hatv_gentrue[i],sigma_gentrue[i],rho_gentrue[i],v_0_gentrue[i])
}

validvector

mean(abs(validvector)[,1])
mean(abs(validvector)[,2])
mean(abs(validvector)[,3])
mean(abs(validvector)[,4])
mean(abs(validvector)[,5])



  
thetaQ<- c(kappa_gen[1],hatv_gen[1],sigma_gen[1],rho_gen[1],v_0_gen[1])
print(Steepestdescent(eps=c(0.0001,0.0001,0.0001,0.0001,0.0001),0.01,theta=thetaQ,2805,0.05,2000)-c(kappa_gentrue[1],hatv_gentrue[1],sigma_gentrue[1],rho_gentrue[1],v_0_gentrue[1]))

Pværdier(0.01,theta=thetaQ,2805,0.05,2000)

Observeretpriser<- Pværdier(0.01,theta=c(kappa_gentrue[1],hatv_gentrue[1],sigma_gentrue[1],rho_gentrue[1],v_0_gentrue[1]),2805,0.05,2000)


Steepestdescent(eps=c(0.0001,0.0001,0.0001,0.0001,0.0001),0.01,theta=thetaQ,2805,0.05,2000)
