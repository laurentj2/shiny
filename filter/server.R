
# Define server
shinyServer(function(input, output) {
	# Create an environment for storing data
  # Create an environment for storing data
  symbol_env <- new.env()
  library(quantmod)
  #*****************************************************************
  # Shared Reactive functions
  # http://rstudio.github.com/shiny/tutorial/#inputs-and-outputs
  #******************************************************************    	
  # Get stock data
  getData <- reactive(function() {  	
    cat('getData was called\n')
    
    data <- new.env()
    for(symbol in c(getStock()) ) {
      if (is.null(symbol_env[[symbol]]))
        tryCatch({
          symbol_env[[symbol]] = getSymbols(symbol, from='2005-01-01', src='yahoo', auto.assign = FALSE)
        }, error = function(e) { stop(paste('Problem getting prices for',symbol)) })
      data[[symbol]] = symbol_env[[symbol]]
    }
    
   # bt.prep(data, align='remove.na')
    bt.prep(data, align='keep.all', dates='2005::2016') 
    data
  })

 
  # Helper fns
  getStock <- reactive(function() { toupper(input$symbol) })
  #getStock2 <- reactive(function() { toupper(input$cash) })

	getBackTest <- reactive(function() { 
		#*****************************************************************
		# Load historical data
		#****************************************************************** 
	  
	  # Copyright: Marc Wildi
	  # 15.01.2012
	  # http://blog.zhaw.ch/idp/sefblog
	  
	  
	  #_____________________________________________________________
	  
	  
	  #gdp_q<-gdp
	  #x<-xh
	  
	  
	  # New 2012-code: computes spectral estimates based on DFT
	  spec_comp<-function(insamp,x,d)
	  {
	    #insamp<-260-anf+1
	    #insamp<-len
	    #  K<-(insamp-1)/2
	    weight_func<-NULL#matrix(nrow=K+1,ncol=dim(x)[2])
	    
	    if (d==1)
	    {
	      K<-length(periodogram_bp(diff(x[1:insamp,1]), 1, insamp-1)$fourtrans)-1
	      weight_func<-1:(K+1)
	      #    if (!GDP_T)
	      #    {
	      weight_func<-periodogram_bp(diff(x[1:insamp,1]), 1, insamp-1)$fourtrans
	      #    } else
	      #    {
	      #      weight_func<-periodogram_bp(diff(gdp_q[1:round(insamp/3)]), 1, round(insamp/3)-1)$fourtrans
	      #      weight_func<-c(weight_func,rep(0,K+1-length(weight_func)))
	      #      weight_func<-weight_func*exp(-1.i*(0:K)*pi*publication_lag/K)
	      #    }
	      # explaining variables                            ts.plot(x[1:insamp,1])
	      if (length(weight_func)>1)
	      {
	        for (j in 2:length(x[1,]))  #j<-2
	        {
	          # Since the data is integrated one uses the pseudo-periodogram: diff(data) and d=1
	          weight_func<-cbind(weight_func,periodogram_bp(diff(x[1:insamp,j]), 1, insamp-1)$fourtrans)
	        }
	      }
	    } else
	    {
	      weight_func<-periodogram_bp((x[1:insamp,1]), 0, insamp)$fourtrans
	      K<-length(periodogram_bp(x[1:insamp,1], 0, insamp)$fourtrans)-1
	      #    if (!GDP_T)
	      #    {
	      weight_func<-periodogram_bp(x[1:insamp,1], 0, insamp)$fourtrans
	      #    } else
	      #    {
	      #      weight_func<-periodogram_bp(gdp_q[1:round(insamp/3)], 0, round(insamp/3))$fourtrans
	      #      weight_func<-c(weight_func,rep(0,K+1-length(weight_func)))
	      #      weight_func<-weight_func*exp(-1.i*(0:K)*pi*publication_lag/K)
	      #    }
	      
	      
	      # explaining variables                            ts.plot(x[1:insamp,1])
	      if (length(weight_func)>1)
	      {
	        for (j in 2:length(x[1,]))  #j<-2
	        {
	          # Since the data is integrated one uses the pseudo-periodogram: diff(data) and d=1
	          weight_func<-cbind(weight_func,periodogram_bp((x[1:insamp,j]), 0, insamp)$fourtrans)
	        }
	      }
	      
	    }
	    dimnames(weight_func)[[2]]<-dimnames(x)[[2]]
	    #weight_func[,1]<-periodogram_bp(diff(gdp[1:insamp]), 1, insamp-1)$fourtrans
	    # if i1<-T then weight_constraint imposes corresponding values of amplitude functions in frequency zero
	    
	    #  ts.plot(abs(weight_func)[,1])
	    return(list(weight_func=weight_func))
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  # DFT (old code but still in use for new 2012-version...)
	  periodogram_bp <- function(x, dd, n.pg)
	  {
	    ## Preparations
	    n.fit  <- length(x)
	    xx     <- x[((n.fit-n.pg+1):n.fit)]
	    npg2   <- (n.pg/2)
	    perall <- 0*0:npg2
	    fourtrans<-perall
	    
	    ## Case without a seasonal component
	    if (dd < 3)
	    {
	      for (j in (1:npg2)) #j<-1
	      {
	        fourtrans[j+1] <- xx%*%exp((1:(2*npg2))*1.i*j*pi/npg2)
	        term2 <- (1-exp(j*1.i*pi/npg2))^(dd)
	        fourtrans[j+1] <- fourtrans[j+1]/(1-min(dd,1)+min(1,dd)*term2)
	        
	        perall[j+1] <- abs(fourtrans[j+1])^2
	      }
	    }
	    
	    ## Case with a seasonal component, special treatment for Pi/6
	    if (dd >= 3)
	    {
	      for (j in (1:npg2)[(-npg2/6)*(1:6)])
	      {
	        fourtrans[j+1] <- xx%*%exp((1:(2*npg2))*1.i*j*pi/npg2)
	        term2 <- abs(1-exp(j*1.i*pi/npg2))^2
	        term3 <- abs(1-exp(12*j*1.i*pi/npg2))^2
	        perall[j+1] <- abs(fourtrans[j+1])/(term2*term3)
	      }
	      perall[(npg2/6)*(1:6)+1] <- max(perall)*100000
	    }
	    
	    ## Output
	    return(list(perall=perall,fourtrans=fourtrans))
	  }
	  # Copyright: Marc Wildi
	  # 15.01.2012
	  # http://blog.zhaw.ch/idp/sefblog
	  
	  
	  #_____________________________________________________________
	  
	  
	  #gdp_q<-gdp
	  #x<-xh
	  
	  # New 2012-code: computes spectral estimates based on DFT
	  spec_comp<-function(insamp,x,d,GDP_T,publication_lag)
	  {
	    #insamp<-260-anf+1
	    #insamp<-len
	    #  K<-(insamp-1)/2
	    weight_func<-NULL#matrix(nrow=K+1,ncol=dim(x)[2])
	    
	    if (d==1)
	    {
	      K<-length(periodogram_bp(diff(x[1:insamp,1]), 1, insamp-1)$fourtrans)-1
	      weight_func<-1:(K+1)
	      #    if (!GDP_T)
	      #    {
	      weight_func<-periodogram_bp(diff(x[1:insamp,1]), 1, insamp-1)$fourtrans
	      #    } else
	      #    {
	      #      weight_func<-periodogram_bp(diff(gdp_q[1:round(insamp/3)]), 1, round(insamp/3)-1)$fourtrans
	      #      weight_func<-c(weight_func,rep(0,K+1-length(weight_func)))
	      #      weight_func<-weight_func*exp(-1.i*(0:K)*pi*publication_lag/K)
	      #    }
	      # explaining variables                            ts.plot(x[1:insamp,1])
	      if (length(weight_func)>1)
	      {
	        for (j in 2:length(x[1,]))  #j<-2
	        {
	          # Since the data is integrated one uses the pseudo-periodogram: diff(data) and d=1
	          weight_func<-cbind(weight_func,periodogram_bp(diff(x[1:insamp,j]), 1, insamp-1)$fourtrans)
	        }
	      }
	    } else
	    {
	      weight_func<-periodogram_bp((x[1:insamp,1]), 0, insamp)$fourtrans
	      K<-length(periodogram_bp(x[1:insamp,1], 0, insamp)$fourtrans)-1
	      #    if (!GDP_T)
	      #    {
	      weight_func<-periodogram_bp(x[1:insamp,1], 0, insamp)$fourtrans
	      #    } else
	      #    {
	      #      weight_func<-periodogram_bp(gdp_q[1:round(insamp/3)], 0, round(insamp/3))$fourtrans
	      #      weight_func<-c(weight_func,rep(0,K+1-length(weight_func)))
	      #      weight_func<-weight_func*exp(-1.i*(0:K)*pi*publication_lag/K)
	      #    }
	      
	      
	      # explaining variables                            ts.plot(x[1:insamp,1])
	      if (length(weight_func)>1)
	      {
	        for (j in 2:length(x[1,]))  #j<-2
	        {
	          # Since the data is integrated one uses the pseudo-periodogram: diff(data) and d=1
	          weight_func<-cbind(weight_func,periodogram_bp((x[1:insamp,j]), 0, insamp)$fourtrans)
	        }
	      }
	      
	    }
	    dimnames(weight_func)[[2]]<-dimnames(x)[[2]]
	    #weight_func[,1]<-periodogram_bp(diff(gdp[1:insamp]), 1, insamp-1)$fourtrans
	    # if i1<-T then weight_constraint imposes corresponding values of amplitude functions in frequency zero
	    
	    #  ts.plot(abs(weight_func)[,1])
	    return(list(weight_func=weight_func))
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  # DFT (old code but still in use for new 2012-version...)
	  periodogram_bp <- function(x, dd, n.pg)
	  {
	    ## Preparations
	    n.fit  <- length(x)
	    xx     <- x[((n.fit-n.pg+1):n.fit)]
	    npg2   <- (n.pg/2)
	    perall <- 0*0:npg2
	    fourtrans<-perall
	    
	    ## Case without a seasonal component
	    if (dd < 3)
	    {
	      for (j in (1:npg2)) #j<-1
	      {
	        fourtrans[j+1] <- xx%*%exp((1:(2*npg2))*1.i*j*pi/npg2)
	        term2 <- (1-exp(j*1.i*pi/npg2))^(dd)
	        fourtrans[j+1] <- fourtrans[j+1]/(1-min(dd,1)+min(1,dd)*term2)
	        
	        perall[j+1] <- abs(fourtrans[j+1])^2
	      }
	    }
	    
	    ## Case with a seasonal component, special treatment for Pi/6
	    if (dd >= 3)
	    {
	      for (j in (1:npg2)[(-npg2/6)*(1:6)])
	      {
	        fourtrans[j+1] <- xx%*%exp((1:(2*npg2))*1.i*j*pi/npg2)
	        term2 <- abs(1-exp(j*1.i*pi/npg2))^2
	        term3 <- abs(1-exp(12*j*1.i*pi/npg2))^2
	        perall[j+1] <- abs(fourtrans[j+1])/(term2*term3)
	      }
	      perall[(npg2/6)*(1:6)+1] <- max(perall)*100000
	    }
	    
	    ## Output
	    return(list(perall=perall,fourtrans=fourtrans))
	  }
	  
	  
	  
	  
	  
	  
	  
	  # New 2012 code: is needed for implementing spectral matrix in new parametrization including regularization
	  spec_mat_comp<-function(weight_func,L,Lag)
	  {
	    K<-length(weight_func[,1])-1
	    weight_h<-weight_func
	    # Frequency zero receives half weight
	    weight_h[1,]<-weight_h[1,]*0.5
	    # Extract DFT target variable (first column)
	    weight_target<-weight_h[,1]
	    # Rotate all DFT's such that weight_target is real (rotation does not alter mean-square error)
	    weight_h<-weight_h*exp(-1.i*Arg(weight_target))
	    weight_target<-weight_target*exp(-1.i*Arg(weight_target))
	    # DFT's explaining variables (target variable can be an explaining variable too)
	    weight_h_exp<-as.matrix(weight_h[,2:(dim(weight_h)[2])])
	    spec_mat<-as.vector(t(as.matrix(weight_h_exp[1,])%*%t(as.matrix(rep(1,L)))))
	    for (j in 1:(K))
	    {
	      omegak<-j*pi/K
	      exp_vec<-exp(1.i*omegak*((0:(L-1))-Lag))
	      spec_mat<-cbind(spec_mat,as.vector(t(as.matrix(weight_h_exp[j+1,])%*%t(as.matrix(exp_vec)))))
	    }
	    dim(spec_mat)
	    return(list(spec_mat=spec_mat))
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  # New 2012-code: Computes regularization matrices and expresses parameters in central-deviance form
	  
	  mat_func<-function(i1,i2,L,weight_h_exp,lambda_decay,lambda_cross,lambda_smooth,Lag,weight_constraint)
	  {
	    if (Lag>(L-1)/2)
	    {
	      print("Lag larger than L/2!!!!! Will be trimmed automtically to L/2 (symmetric filter)")
	      Lag<-as.integer(L/2)
	    }
	    lambda_smooth<-abs(lambda_smooth)
	    lambda_cross<-abs(lambda_cross)
	    lambda_decay<-abs(lambda_decay)
	    # The smoothness and decay regularization are conveniently (rightly) implemented on original parameters
	    # The Q_smooth and Q_decay matrices address regularizations for original unconstrained parameters (Therefore dimension L^2)
	    # At the end, the matrix des_mat is used to map these regularizations to central-deviance parameters
	    # accounting for first order constraints!!!!!!!!!!!!!!!!!!!
	    Q_smooth<-matrix(data=rep(0,((L)*length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
	    Q_decay<-matrix(data=rep(0,((L)*length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
	    # Cross-sectional regularization if dimension>1
	    if ((length(weight_h_exp[1,])>1))
	    {
	      # The cross-sectional regularization is conveniently implemented on central-deviance parameters. The regularization is expressed on the
	      # unconstrained central-deviance parameters (dimension L), then mapped to the original (unconstrained) parameters (dimension L) with Q_centraldev_original
	      # and then maped back to central-deviance with constraint (dim L-1) with des_mat (mathematically unnecessarily complicate but more convenient to implement in code).
	      Q_cross<-matrix(data=rep(0,((L)*length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
	      Q_centraldev_original<-matrix(data=rep(0,((L)*length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
	    }
	    for (i in 1:L)
	    {
	      # For symmetric filters or any historical filter with Lag>0 the decay must be symmetric about b_max(0,Lag) zunehmen
	      Q_decay[i,i]<-lambda_decay*(1+lambda_decay)^(2*abs(i-1-max(0,Lag)))
	      if(i==1)
	      {
	        Q_smooth[i,i:(i+2)]<-lambda_smooth*c(1,-2,1)
	      } else
	      {
	        if(i==2)
	        {
	          Q_smooth[i,(i-1):(i+2)]<-lambda_smooth*c(-2,5,-4,1)
	        } else
	        {
	          if(i==L)
	          {
	            Q_smooth[i,(i-2):i]<-lambda_smooth*c(1,-2,1)
	          } else
	          {
	            if(i==L-1)
	            {
	              Q_smooth[i,(i-2):(i+1)]<-lambda_smooth*c(1,-4,5,-2)
	            } else
	            {
	              Q_smooth[i,(i-2):(i+2)]<-lambda_smooth*c(1,-4,6,-4,1)
	            }
	          }
	        }
	      }
	    }
	    
	    if (length(weight_h_exp[1,])>1)
	    {
	      for (j in 1:max(1,(length(weight_h_exp[1,])-1)))   #j<-1
	      {
	        Q_smooth[j*L+1:L,j*L+1:L]<-Q_smooth[1:L,1:L]
	        Q_decay[j*L+1:L,j*L+1:L]<-Q_decay[1:L,1:L]
	      }
	      diag(Q_centraldev_original[1:L,1:L])<-rep(1,L)
	      diag(Q_centraldev_original[1:L,L+1:L])<-rep(-1,L)
	      for (i in 2:length(weight_h_exp[1,]))
	      {
	        diag(Q_centraldev_original[(i-1)*L+1:L,1:L])<-rep(1,L)
	        diag(Q_centraldev_original[(i-1)*L+1:L,(i-1)*L+1:L])<-rep(1,L)
	        diag(Q_centraldev_original[1:L,(i-1)*L+1:L])<-rep(-1,L)
	      }
	      
	      Q_centraldev_original<-solve(Q_centraldev_original)
	      diag(Q_cross[L+1:((length(weight_h_exp[1,])-1)*L),L+1:((length(weight_h_exp[1,])-1)*L)])<-
	        lambda_cross*rep(1,((length(weight_h_exp[1,])-1)*L))
	    }
	    
	    # weight vector (relevant if i1<-T)
	    if (i1)
	    {
	      if (i2)
	      {
	        # Modifications 17.04.2012 : the definition of the vector w_eight has been generalized:
	        #               The new definition allows to impose constraints to b_Lag, b_{Lag+1} instead of b_{L-1} and b_L
	        #               Therefore the decay regularization does not potentially conflict with filter constraints
	        if (Lag<1)
	        {
	          w_eight<-c(-(Lag-1)*weight_constraint[1],Lag*weight_constraint[1],rep(0,L-2))
	        } else
	        {
	          w_eight<-c(rep(0,Lag),weight_constraint[1],rep(0,L-Lag-1))
	        }
	        
	        if (length(weight_h_exp[1,])>1)
	        {
	          for (j in 2:length(weight_h_exp[1,]))
	          {
	            
	            if (Lag<1)
	            {
	              w_eight<-c(w_eight,-(Lag-1)*weight_constraint[j],Lag*weight_constraint[j],rep(0,L-2))
	            } else
	            {
	              w_eight<-c(w_eight,c(rep(0,Lag),weight_constraint[j],rep(0,L-Lag-1)))
	            }
	          }
	        }
	      } else
	      {
	        if (Lag<1)
	        {
	          w_eight<-c(weight_constraint[1],rep(0,L-1))
	        } else
	        {
	          w_eight<-c(rep(0,Lag),weight_constraint[1],rep(0,L-Lag-1))
	        }
	        if (length(weight_h_exp[1,])>1)
	        {
	          for (j in 2:length(weight_h_exp[1,]))
	          {
	            if (Lag<1)
	            {
	              w_eight<-c(w_eight,weight_constraint[j],rep(0,L-1))
	            } else
	            {
	              w_eight<-c(w_eight,rep(0,Lag),weight_constraint[j],rep(0,L-Lag-1))
	            }
	          }
	        }
	      }
	    } else
	    {
	      w_eight<-rep(0,L*length(weight_h_exp[1,]))
	    }
	    
	    # Here we implement the matrix which links central-deviance parameters and original parameters
	    # Modifications 17.04.2012: we generalize the definition of des_mat in the case of i2<-T (i2<-F is OK)
	    if (i2)
	    {
	      if (i1)
	      {
	        # First and second order restrictions
	        des_mat<-matrix(data=rep(0,(L-2)*L*(length(weight_h_exp[1,]))^2),nrow=(L-2)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
	        
	        for (i in 1:(L-2))
	        {
	          if (Lag<1)
	          {
	            des_mat[i,i+2+(0:(length(weight_h_exp[1,])-1))*L]<-1
	            des_mat[i,1+(0:(length(weight_h_exp[1,])-1))*L]<-i
	            des_mat[i,2+(0:(length(weight_h_exp[1,])-1))*L]<--(i+1)
	            
	          } else
	          {
	            des_mat[i,ifelse(i<Lag+1,i,i+2)+(0:(length(weight_h_exp[1,])-1))*L]<-1
	            des_mat[i,Lag+1+(0:(length(weight_h_exp[1,])-1))*L]<-ifelse(i<Lag+1,-(Lag+2-i),i-Lag)
	            des_mat[i,Lag+2+(0:(length(weight_h_exp[1,])-1))*L]<-ifelse(i<Lag+1,(Lag+1-i),-(i-Lag+1))
	          }
	        }
	        if (length(weight_h_exp[1,])>1)
	        {
	          for (j in 1:max(1,(length(weight_h_exp[1,])-1)))
	          {
	            for (i in 1:(L-2))                                                          #reg_mat[600,600
	            {
	              
	              if (Lag<1)
	              {
	                des_mat[i+j*(L-2),i+2]<--1
	                des_mat[i+j*(L-2),1]<--i
	                des_mat[i+j*(L-2),2]<-(i+1)
	              } else
	              {
	                des_mat[i+j*(L-2),ifelse(i<Lag+1,i,i+2)]<--1
	                des_mat[i+j*(L-2),Lag+1]<--ifelse(i<Lag+1,-(Lag+2-i),i-Lag)
	                des_mat[i+j*(L-2),Lag+2]<--ifelse(i<Lag+1,(Lag+1-i),-(i-Lag+1))
	              }
	              
	              if (Lag<1)
	              {
	                des_mat[i+j*(L-2),i+2+j*L]<-1
	                des_mat[i+j*(L-2),1+j*L]<-i
	                des_mat[i+j*(L-2),2+j*L]<--(i+1)
	              } else
	              {
	                des_mat[i+j*(L-2),ifelse(i<Lag+1,i+j*L,i+2+j*L)]<-1
	                des_mat[i+j*(L-2),Lag+1+j*L]<-ifelse(i<Lag+1,-(Lag+2-i),i-Lag)
	                des_mat[i+j*(L-2),Lag+2+j*L]<-ifelse(i<Lag+1,(Lag+1-i),-(i-Lag+1))
	              }
	            }
	          }
	        }
	      } else
	      {
	        # Modifications 17.04.2012: we generalize the definition of des_mat in the case of i2<-T (i2<-F is OK)
	        des_mat<-matrix(data=rep(0,(L-1)*L*(length(weight_h_exp[1,]))^2),nrow=(L-1)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
	        for (i in 1:(L-1))      #Lag<-2
	        {
	          
	          if (Lag<1)
	          {
	            # For Lag<1 we have to impose the restriction to b2 (b1 does not appear in the i2<-T and i1<-F case!)
	            des_mat[i,ifelse(i<2,i,i+1)+(0:(length(weight_h_exp[1,])-1))*L]<-1
	            des_mat[i,1+1+(0:(length(weight_h_exp[1,])-1))*L]<-ifelse(i<2,-(i-1),-i)/(1)
	            
	          } else
	          {
	            des_mat[i,ifelse(i<Lag+2,i,i+1)+(0:(length(weight_h_exp[1,])-1))*L]<-1
	            des_mat[i,Lag+2+(0:(length(weight_h_exp[1,])-1))*L]<-ifelse(i<Lag+2,Lag+1-i,Lag-i)
	          }
	        }
	        # new 08.02.2012: the if loop is new/modified
	        if (length(weight_h_exp[1,])>1)
	        {
	          for (j in 1:max(1,(length(weight_h_exp[1,])-1)))
	          {
	            for (i in 1:(L-1))
	            {
	              
	              if (Lag<1)
	              {
	                # For Lag<1 we have to impose the restriction to b2 (b1 does not appear in the i2<-T and i1<-F case!)
	                des_mat[i+j*(L-1),ifelse(i<2,i,i+1)]<--1
	                des_mat[i+j*(L-1),1+1]<--ifelse(i<1+1,-(i-1),-i)/(1)
	                
	              } else
	              {
	                des_mat[i+j*(L-1),ifelse(i<Lag+2,i,i+1)]<--1
	                des_mat[i+j*(L-1),Lag+2]<--ifelse(i<Lag+2,Lag+1-i,Lag-i)
	              }
	              
	              if (Lag<1)
	              {
	                # For Lag<1 we have to impose the restriction to b2 (b1 does not appear in the i2<-T and i1<-F case!)
	                des_mat[i+j*(L-1),ifelse(i<2,i,i+1)+j*L]<-1
	                des_mat[i+j*(L-1),1+1+j*L]<-ifelse(i<1+1,-(i-1),-i)/(1)
	                
	              } else
	              {
	                des_mat[i+j*(L-1),ifelse(i<Lag+2,i,i+1)+j*L]<-1
	                des_mat[i+j*(L-1),Lag+2+j*L]<-ifelse(i<Lag+2,Lag+1-i,Lag-i)
	              }
	              
	            }
	          }
	        }
	        
	      }
	    } else
	    {
	      if (i1)                            #lambda_cross=0
	      {
	        des_mat<-matrix(data=rep(0,(L-1)*L*(length(weight_h_exp[1,]))^2),nrow=(L-1)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
	        # The cross-sectional regularization can be directly implemented into reg_mat (it adresses the central deviance parameters)
	        for (i in 1:(L-1))  #i<-1
	        {
	          # The i1-constraint is imposed on b_max(0,Lag) (instead of b_L as in original procedure) in order to avoid a conflict with the exponential decay requirement
	          if (Lag<1)
	          {
	            des_mat[i,i+1+(0:(length(weight_h_exp[1,])-1))*L]<-1
	            des_mat[i,1+(0:(length(weight_h_exp[1,])-1))*L]<--1
	          } else
	          {
	            # Lag cannot be larger than (L-1)/2 (symmetric filter)
	            des_mat[i,ifelse(i<Lag+1,i,i+1)+(0:(length(weight_h_exp[1,])-1))*L]<-1
	            des_mat[i,Lag+1+(0:(length(weight_h_exp[1,])-1))*L]<--1
	          }
	        }
	        # new 08.02.2012: the if loop is new/modified
	        if (length(weight_h_exp[1,])>1)
	        {
	          for (j in 1:max(1,(length(weight_h_exp[1,])-1)))   #j<-1
	          {
	            for (i in 1:(L-1))
	            {
	              # The i1-constraint is imposed on b_max(0,Lag) (instead of b_L as in original procedure) in order to avoid a conflict with the exponential decay requirement
	              if (Lag<1)
	              {
	                des_mat[i+j*(L-1),i+1]<--1
	                des_mat[i+j*(L-1),1]<-1
	                des_mat[i+j*(L-1),i+1+j*L]<-1
	                des_mat[i+j*(L-1),1+j*L]<--1
	              } else
	              {
	                # Lag cannot be larger than (L-1)/2 (symmetric filter)
	                des_mat[i+j*(L-1),ifelse(i<Lag+1,i,i+1)]<--1
	                des_mat[i+j*(L-1),Lag+1]<-1
	                des_mat[i+j*(L-1),ifelse(i<Lag+1,i,i+1)+j*L]<-1
	                des_mat[i+j*(L-1),Lag+1+j*L]<--1
	              }
	              # The cross sectional regularization is implemented directly into reg_mat (it addresses central deviance parameters!)
	              #            reg_mat[i+(j)*(L-1),i+(j)*(L-1)]<-lambda_cross
	            }
	          }             # det(reg_mat)         lambda_cross<-0.3
	        }
	      } else
	      {
	        des_mat<-matrix(data=rep(0,(L)*L*(length(weight_h_exp[1,]))^2),nrow=(L)*length(weight_h_exp[1,]),ncol=(L)*length(weight_h_exp[1,]))
	        for (i in 1:(L))
	        {
	          des_mat[i,i+(0:(length(weight_h_exp[1,])-1))*L]<-1
	        }
	        # new 08.02.2012: the if loop is new/modified
	        if (length(weight_h_exp[1,])>1)
	        {
	          
	          for (j in 1:max(1,(length(weight_h_exp[1,])-1)))
	          {
	            for (i in 1:(L))
	            {
	              des_mat[i+(j)*(L),i]<--1
	              des_mat[i+(j)*(L),i+j*L]<-1
	            }
	          }
	        }
	      }
	    }
	    # Here we fold all three regularizations (cross, smooth and decay) into a single reg-matrix
	    # The smoothness and decay terms address original parameters and must be transformed (by des_mat) in order to
	    # conform to the central-deviance parameterization as well as to first (and/or second) order constraints
	    
	    ## begin new 08.02.2012: the if-loop is new
	    if ((length(weight_h_exp[1,])>1))
	    {
	      reg_t<-(Q_smooth+Q_decay+t(Q_centraldev_original)%*%Q_cross%*%Q_centraldev_original)
	    } else
	    {
	      reg_t<-(Q_smooth+Q_decay)
	    }
	    
	    
	    reg_mat<-(des_mat)%*%reg_t%*%t(des_mat)
	    reg_xtxy<-des_mat%*%reg_t%*%w_eight#+t(w_eight)%*%reg_t%*%t(des_mat)
	    
	    
	    ## end new 08.02.2012
	    return(list(des_mat=des_mat,reg_mat=reg_mat,reg_xtxy=reg_xtxy,w_eight=w_eight))
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  # New 2012 I-MDFA code: might crash if dimension is 1 (univariate setting): I did not test this!!!
	  # Generalizes old code (below) when regularization parameters are set to zero
	  # This new function can deal with very richly parametrized designs (high-dimensional multivariate with long lag structures)
	  # Regularization affect/control smoothness, rate of decay and cross-sectional similarity of filter parameters/weights
	  
	  
	  mdfa_analytic_new<-function(K,L,lambda,weight_func,Lag,Gamma,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth,lin_expweight)
	  {
	    # In order to enhance numerical speed this call could be done outside (as long as L and Lag are fixed)
	    spec_mat<-spec_mat_comp(weight_func,L,Lag)$spec_mat     #dim(spec_mat[,1])
	    # weighting of amplitude function in stopband
	    omega_Gamma<-as.integer(cutoff*K/pi)
	    if ((K-omega_Gamma+1)>0)
	    {
	      if (lin_expweight)
	      {
	        expweight_vec<-c(rep(1,omega_Gamma),1+rep(expweight,K-omega_Gamma+1))
	      } else
	      {
	        expweight_vec<-c(rep(1,omega_Gamma),(1:(K-omega_Gamma+1))^(expweight/2))
	        #      expweight_vec<-c(rep(1,omega_Gamma),1+(1:(K-omega_Gamma+1))*(expweight/2))
	      }
	      weight_h<-weight_func*expweight_vec
	    } else
	    {
	      expweight_vec<-rep(1,K+1)
	      weight_h<-weight_func* expweight_vec
	    }
	    #ts.plot(abs(weight_h))       ts.plot(Gamma)
	    # Frequency zero receives half weight
	    weight_h[1,]<-weight_h[1,]*0.5
	    # DFT target variable
	    weight_target<-weight_h[,1]
	    # Rotate all DFT's such that weight_target is real (rotation does not alter mean-square error)
	    weight_h<-weight_h*exp(-1.i*Arg(weight_target))
	    weight_target<-Re(weight_target*exp(-1.i*Arg(weight_target)))
	    # DFT's explaining variables: target variable can be an explaining variable too
	    weight_h_exp<-as.matrix(weight_h[,2:(dim(weight_h)[2])])
	    
	    # The spectral matrix is inflated in stopband: effect of expweight
	    spec_mat<-t(t(spec_mat)*expweight_vec) #dim(spec_mat)
	    
	    # Compute design matrix and regularization matrix
	    
	    mat_obj<-mat_func(i1,i2,L,weight_h_exp,lambda_decay,lambda_cross,lambda_smooth,Lag,weight_constraint)
	    
	    des_mat<-mat_obj$des_mat
	    reg_mat<-mat_obj$reg_mat           #dim(des_mat)
	    reg_xtxy<-mat_obj$reg_xtxy
	    w_eight<-mat_obj$w_eight
	    
	    # Solve estimation problem in space of b_main and b_dev
	    mat_x<-des_mat%*%spec_mat          #dim(mat_x)    dim(spec_mat)   length(w_eight)
	    X_new<-t(Re(mat_x))+sqrt(1+Gamma*lambda)*1.i*t(Im(mat_x))
	    # xtx can be written either in Conj(X_new)%*%X_new or as below:
	    xtx<-t(Re(X_new))%*%Re(X_new)+t(Im(X_new))%*%Im(X_new)
	    # The filter restrictions (i1<-T) appear as constants on the right hand-side of the equation:
	    xtxy<-t(Re(t(w_eight)%*%spec_mat)%*%Re(t(spec_mat)%*%t(des_mat))+
	              Im(t(w_eight)%*%t(t(spec_mat)*sqrt(1+Gamma*lambda)))%*%Im(t(t(t(spec_mat)*sqrt(1+Gamma*lambda)))%*%t(des_mat)))
	    # scaler makes scales of regularization and unconstrained optimization `similar'
	    scaler<-mean(diag(xtx))
	    X_inv<-solve(xtx+scaler*reg_mat)
	    bh<-as.vector(X_inv%*%(((t(Re(X_new)*weight_target))%*%Gamma)-xtxy-scaler*reg_xtxy))
	    # the last two filter weights are functions of the previous ones through the first and second order restrictions
	    b<-matrix(nrow=L,ncol=length(weight_h_exp[1,]))
	    # Reconstruct original parameters
	    bhh<-t(des_mat)%*%bh
	    for (k in 1:L) #k<-1
	    {      #dim(t(des_mat))
	      b[k,]<-bhh[(k)+(0:(length(weight_h_exp[1,])-1))*L]
	    }
	    
	    # Modifications 17.04.2012 : the newly defined vector w_eight allows for simple/straightforward adjustment of filter coefficients
	    weight_cm<-matrix(w_eight,ncol=(length(weight_h_exp[1,])))
	    # Add level constraints (if i1<-F then this matrix is zero)
	    b<-b+weight_cm
	    
	    # The following derivations of the DFA-criterion are all equivalent
	    # They are identical with rever as calculated at the end of the function except if i1<-T and weight_constraint different from zero
	    # In the latter case an additional constant interfers with the estimation
	    
	    # The target Y in the frequency-domain is the real vector weight_target*Gamma (both vectors are real)
	    # The Regression estimate (the smoother) of Y is the following expression:
	    trth<-((X_new)%*%(X_inv%*%t(Re(X_new))))%*%(weight_target*Gamma)
	    # This expression is identical to trt computed below if lambda=0 (assuming i1<-F or weight_constraint=0); otherwise trth is identical to Re(trt)+1.i*sqrt(1+lambda*Gamma)*Im(trt))
	    # The projection matrix is therefore:
	    Proj_mat<-((X_new)%*%(X_inv%*%t(Re(X_new))))              #dim(Proj_mat)
	    # The residual projection matrix is
	    res_mat<-diag(rep(1,dim(Proj_mat)[1]))-Proj_mat
	    # DFA criterion: first possibility (all three variants are identical)
	    sum(abs(res_mat%*%(weight_target*Gamma))^2)
	    #  ts.plot(abs(res_mat%*%(weight_target*Gamma))^2)
	    #  acf(abs(res_mat%*%(weight_target*Gamma)))
	    # Residuals
	    resi<-res_mat%*%(weight_target*Gamma)
	    # DFA criterion: second possibility
	    t(Conj(resi))%*%resi
	    t((weight_target*Gamma))%*%(t(Conj(res_mat))%*%(res_mat))%*%(weight_target*Gamma)
	    
	    #  sum(diag(res_mat))
	    #  sum(diag(t(res_mat%*%Conj(res_mat))))
	    
	    # The interesting `effective degrees of freedom' used here emphasizes an unbiased estimate of the mean-square residual
	    #    See  http://en.wikipedia.org/wiki/Degrees_of_freedom_(statistics) (effective degrees of freedom: the expression tr((I-H)(I-H)')
	    #    Note that res_mat=I-H see above
	    #    Then (Y-Y^)(Y-Y^)/tr((I-H)(I-H)') is an unbiased estimate of the mean-square residual error (in our case Y=weight_target*Gamma. see above)
	    #    This correcting `effective degrees of freedom' can then be used to implement a generalized AIC, see below
	    #    Neither of the other proposed definitions of `effective degrees of freedom' on Wiki-site tackle this problem (in particular the trace of the smoothing operator is not what we want!!!!)
	    degrees_freedom<-2*Re(sum(diag(t(Conj(res_mat))%*%(res_mat))))-1
	    freezed_degrees<-2*K+1-degrees_freedom
	    #  degrees_freedom<-K-Re(sum(diag(Proj_mat)))
	    # DFA Criterion: third possibility (here an additional normalization by 1/(2*(K+1)^2))
	    sum(abs(Gamma*weight_target-trth)^2)
	    
	    
	    #ts.plot(b)
	    # Transferfunction
	    trffkt<-matrix(nrow=K+1,ncol=length(weight_h_exp[1,]))
	    trffkth<-trffkt
	    trffkt[1,]<-apply(b,2,sum)
	    trffkth[1,]<-trffkt[1,]
	    #  b<-scale(b,center=T,scale=F)
	    
	    for (j in 1:length(weight_h_exp[1,]))
	    {
	      for (k in 0:(K))#k<-1
	      {
	        trffkt[k+1,j]<-(b[,j]%*%exp(1.i*k*(0:(L-1))*pi/(K)))
	      }
	    }
	    trt<-apply(((trffkt)*exp(1.i*(0-Lag)*pi*(0:(K))/K))*weight_h_exp,1,sum)
	    # DFA criterion which accounts for customization but not for regularization term
	    rever<-sum(abs(Gamma*weight_target-Re(trt)-1.i*sqrt(1+lambda*Gamma)*Im(trt))^2)/(2*(K+1)^2)
	    # MS-filter error : DFA-criterion without effects by lambda or expweight (one must divide spectrum by expweight_vec)
	    MS_error<-sum((abs(Gamma*weight_target-trt)/expweight_vec)^2)/(2*(K+1)^2)
	    # Definition of Accuracy, time-shift and noise suppression terms
	    # Please note that:
	    #       1. these terms refer to the original non-linearized criterion: therefore they do not sum up to rever
	    #       2. we are interested in decomposing the mean-square error i.e. we ignore expweight and lambda here (we just want to measure the impact of lambda and expweight)
	    #               Therefore, we use the DFT without expweight_vec i.e. we have to divide all spectral constents by expweight_vec
	    Gamma_cp<-Gamma[1+0:as.integer(K*(cutoff/pi))]
	    Gamma_cn<-Gamma[(2+as.integer(K*(cutoff/pi))):(K+1)]
	    trt_cp<-(trt/expweight_vec)[1+0:as.integer(K*(cutoff/pi))]
	    trt_cn<-(trt/expweight_vec)[(2+as.integer(K*(cutoff/pi))):(K+1)]
	    weight_target_cp<-(weight_target/expweight_vec)[1+0:as.integer(K*(cutoff/pi))]
	    weight_target_cn<-(weight_target/expweight_vec)[(2+as.integer(K*(cutoff/pi))):(K+1)]
	    # define singular observations
	    Accuracy<-sum(abs(Gamma_cp*weight_target_cp-abs(trt_cp))^2)/(2*(K+1)^2)
	    Timeliness<-4*sum(abs(Gamma_cp)*abs(trt_cp)*sin(Arg(trt_cp)/2)^2*weight_target_cp)/(2*(K+1)^2)
	    Smoothness<-sum(abs(Gamma_cn*weight_target_cn-abs(trt_cn))^2)/(2*(K+1)^2)
	    Shift_stopband<-4*sum(abs(Gamma_cn)*abs(trt_cn)*sin(Arg(trt_cn)/2)^2*weight_target_cn)/(2*(K+1)^2)
	    # Check: the following expression should vanish
	    Accuracy+Timeliness+Smoothness+Shift_stopband-MS_error
	    # Very prototypical: AIC
	    #  aic<-ifelse(degrees_freedom<K+1&degrees_freedom>1,log(rever)+2*(K-degrees_freedom)/(K)+2*(K-degrees_freedom)*(K-degrees_freedom+1)/(K*(degrees_freedom-1)),NA)
	    aic<-ifelse(degrees_freedom<K+1&degrees_freedom>1,log(rever)+2*(K-degrees_freedom+1)/(degrees_freedom-2),NA)
	    
	    return(list(b=b,trffkt=trffkt,rever=rever,degrees_freedom=degrees_freedom,aic=aic,freezed_degrees=freezed_degrees,Accuracy=Accuracy,Smoothness=Smoothness,Timeliness=Timeliness,MS_error=MS_error))
	  }
	  
	  
	  
	  #trffkt<-trffkt_cust
	  #weight_func<-weight_func_cust
	  
	  
	  
	  
	  
	  MS_decomp_total<-function(Gamma,trffkt,weight_func,cutoff,Lag)     #trffkt<-trffkt_hp         pi/ub
	  {
	    # If lengths of spectrum and/or Gamma and/or Gamma^ do not match (for example when mixing entries from TRAMO and DFA) then series are reconciled
	    if (!(length(trffkt[,1])==length(weight_func[,1])))
	    {
	      len_w<-min(length(trffkt[,1]),length(weight_func[,1]))
	      if (length(trffkt[,1])<length(weight_func[,1]))
	      {
	        len_r<-(length(weight_func[,1])-1)/(length(trffkt[,1])-1)
	        weight_funch<-weight_func[c(1,(1:(len_w-1))*len_r),]
	        trffkth<-trffkt
	      } else
	      {
	        len_r<-1/((length(weight_func[,1])-1)/(length(trffkt[,1])-1))
	        trffkth<-trffkt[c(1,(1:(len_w-1))*len_r),]
	        weight_funch<-weight_func
	      }
	    } else
	    {
	      len_w<-length(trffkt[,1])
	      weight_funch<-weight_func
	      trffkth<-trffkt
	      Gammah<-Gamma
	    }
	    if (length(Gamma)>len_w)
	    {
	      len_r<-(length(Gamma)-1)/(len_w-1)
	      Gammah<-Gamma[c(1,(1:(len_w-1))*len_r)]
	    } else
	    {
	      Gammah<-Gamma
	    }
	    
	    #cbind(Ghh,Gammah)
	    
	    weight_h<-weight_funch
	    K<-length(weight_funch[,1])-1
	    weight_target<-weight_h[,1]
	    # Rotate all DFT's such that weight_target is real (rotation does not alter mean-square error)
	    weight_h<-weight_h*exp(-1.i*Arg(weight_target))
	    weight_target<-Re(weight_target*exp(-1.i*Arg(weight_target)))
	    # DFT's explaining variables: target variable can be an explaining variable too
	    weight_h_exp<-as.matrix(weight_h[,2:(dim(weight_h)[2])])
	    
	    trt<-apply(((trffkth)*exp(1.i*(0-Lag)*pi*(0:(K))/K))*weight_h_exp,1,sum)
	    # MS-filter error : DFA-criterion without effects by lambda or expweight (one must divide spectrum by expweight_vec)
	    MS_error<-sum((abs(Gammah*weight_target-trt))^2)/(2*(K+1)^2)
	    # Definition of Accuracy, time-shift and noise suppression terms
	    # Please note that:
	    #       1. these terms refer to the original non-linearized criterion: therefore they do not sum up to rever
	    #       2. we are interested in decomposing the mean-square error i.e. we ignore expweight and lambda here (we just want to measure the impact of lambda and expweight)
	    #               Therefore, we use the DFT without expweight_vec i.e. we have to divide all spectral constents by expweight_vec
	    Gamma_cp<-Gammah[1+0:as.integer(K*(cutoff/pi))]
	    Gamma_cn<-Gammah[(2+as.integer(K*(cutoff/pi))):(K+1)]
	    trt_cp<-trt[1+0:as.integer(K*(cutoff/pi))]
	    trt_cn<-trt[(2+as.integer(K*(cutoff/pi))):(K+1)]
	    weight_target_cp<-weight_target[1+0:as.integer(K*(cutoff/pi))]
	    weight_target_cn<-weight_target[(2+as.integer(K*(cutoff/pi))):(K+1)]
	    # define singular observations
	    Accuracy<-sum(abs(Gamma_cp*weight_target_cp-abs(trt_cp))^2)/(2*(K+1)^2)
	    Timeliness<-4*sum(abs(Gamma_cp)*abs(trt_cp)*sin(Arg(trt_cp)/2)^2*weight_target_cp)/(2*(K+1)^2)
	    Smoothness<-sum(abs(Gamma_cn*weight_target_cn-abs(trt_cn))^2)/(2*(K+1)^2)
	    Shift_stopband<-4*sum(abs(Gamma_cn)*abs(trt_cn)*sin(Arg(trt_cn)/2)^2*weight_target_cn)/(2*(K+1)^2)
	    # Check: the following expression should vanish
	    Accuracy+Timeliness+Smoothness+Shift_stopband-MS_error
	    # Very prototypical: AIC
	    #  aic<-ifelse(degrees_freedom<K+1&degrees_freedom>1,log(rever)+2*(K-degrees_freedom)/(K)+2*(K-degrees_freedom)*(K-degrees_freedom+1)/(K*(degrees_freedom-1)),NA)
	    
	    return(list(Accuracy=Accuracy,Smoothness=Smoothness,Timeliness=Timeliness,MS_error=MS_error))
	  }
	  
	  
	  
	  IMDFA_comp<-function(Lag,K,L,lambda,weight_func,Gamma,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth,x,plots,lin_expweight)
	  {
	    
	    Lag_IMDFA<-Lag
	    
	    i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lag_IMDFA,Gamma,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth,lin_expweight)
	    
	    print(paste("freezed degrees: ",round(i_mdfa$freezed_degrees)-1,sep=""))
	    print(paste("criterion: ",i_mdfa$rever,sep=""))
	    
	    if (plots)
	    {
	      colo<-rainbow(length(i_mdfa$b[1,]))
	      par(mfrow=c(2,1))
	      mplot<-abs(i_mdfa$trffkt)
	      ymax<-max(mplot)
	      ymin<-min(mplot)
	      plot(mplot[,1],ylim=c(ymin,ymax),col=colo[1],type="l")
	      mtext(line=-1,dimnames(x)[[2]][2],col=colo[1])
	      if (dim(mplot)[2]>1)
	      {
	        for (i in 2:length(mplot[1,]))
	        {
	          lines(mplot[,i],col=colo[i])
	          mtext(line=-i,dimnames(x)[[2]][i+1],col=colo[i])
	        }
	      }
	      mplot<-i_mdfa$b
	      ymax<-max(mplot)
	      ymin<-min(mplot)
	      plot(mplot[,1],ylim=c(ymin,ymax),col=colo[1],type="l")
	      mtext(line=-1,dimnames(x)[[2]][2],col=colo[1])
	      if (dim(mplot)[2]>1)
	      {
	        for (i in 2:length(mplot[1,]))
	        {
	          lines(mplot[,i],col=colo[i])
	          mtext(line=-i,dimnames(x)[[2]][i+1],col=colo[i])
	        }
	      }
	    }
	    # First order constraint
	    print(paste("First order restriction: ",apply(i_mdfa$b,2,sum),sep=""))
	    # second order constraint
	    print(paste("Second order constraint: ",apply(i_mdfa$b*(0:(L-1)),2,sum),sep=""))
	    
	    
	    
	    xff<-matrix(nrow=dim(as.matrix(x[,-1]))[1],ncol=dim(as.matrix(x[,-1]))[2])
	    
	    bn<-i_mdfa$b
	    #  ts.plot(bn,lty=1:7)
	    for (i in 1:length(x[,1])) #i<-20
	    {
	      xff[i,]<-0
	      for (j in 2:(length(x[1,])))  #j<-2
	      {
	        xff[i,j-1]<-xff[i,j-1]+bn[1:min(i,L),j-1]%*%x[i:max(1,i-L+1),j]
	      }
	    }
	    return(list(xff=xff,i_mdfa=i_mdfa))
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  aic_reg_comp<-function(i1,i2,expweight,lambda,L,spec_mat,l_s_vec,l_c_vec,l_l_vec,K,weight_func,Lagh,Gammah,cutoff,weight_constraint,lambda_decay,x,x_sym,len,cli)
	  {
	    aic_array<-array(dim=c(length(l_s_vec),length(l_c_vec),length(l_l_vec)))
	    df_array<-aic_array
	    rever_array<-aic_array
	    b_array<-array(list(NULL), dim=c(length(l_s_vec),length(l_c_vec),length(l_l_vec)))
	    for (i in 1:length(l_s_vec))   #i<-1  j<-1  k<-1
	    {
	      for (j in 1:length(l_c_vec))
	      {
	        for (k in 1:length(l_l_vec))
	        {
	          lambda_smooth<-l_s_vec[i]
	          lambda_cross<-l_c_vec[j]
	          lambda_decay<-l_l_vec[k]
	          
	          i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth)
	          
	          aic_array[i,j,k]<-i_mdfa$aic
	          df_array[i,j,k]<-i_mdfa$degrees_freedom
	          rever_array[i,j,k]<-i_mdfa$rever
	          b_array[i,j,k]<-list(b=i_mdfa$b)
	        }
	      }
	      print(i)
	    }
	    return(list(aic_array=aic_array,df_array=df_array,rever_array=rever_array,b_array=b_array))
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  best_reg<-function(df_target,K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,spec_mat,magn,anzsup)
	  {
	    
	    
	    other.obj<-list(K=K,L=L,lambda=lambda,weight_func=weight_func,Lagh=Lagh,Gammah=Gammah,expweight=expweight,
	                    cutoff=cutoff,i1=i1,i2=i2,weight_constraint=weight_constraint,spec_mat=spec_mat,lambda_smooth=lambda_smooth,
	                    lambda_cross=lambda_cross,df_target=df_target)
	    
	    co   <- list(REPORT=0, trace=0, maxit=1000)
	    lambda_init<-0.02
	    
	    # The starting value is selected such that lambda_smooth=lambda_decay=lambda_cross (=lambda_init) generate
	    # a filter with df_target degrees of freedom (equally weighted in the regularization troika)
	    df_init_obj <- optim(lambda_init, df_init,control=co, other.obj=other.obj)
	    
	    lambda_init<-df_init_obj$par
	    save(lambda_init,file=paste(path.out,"lambda_init_",Lagh,sep=""))
	    lambda_mat<-matrix(ncol=2*anzsup+1,nrow=2*anzsup+1)
	    df_mat<-matrix(ncol=2*anzsup+1,nrow=2*anzsup+1)
	    rever_mat<-matrix(ncol=2*anzsup+1,nrow=2*anzsup+1)
	    i_ch<-0
	    # The hyperplane in regularization space is developped around the above lambda_init `equally weighted solution'
	    
	    
	    for (i in 0:(anzsup))   #i<-anzsup
	    {
	      if (i_ch==0)
	      {
	        lambda_smooth<-max(0,(lambda_init*(1+magn*i/10)))
	        j_ch<-0
	        for (j in 0:anzsup)      #j<-0
	        {
	          # it is checked whether the point in the plane is admissible i.e. whether lambda_decay has meaningful values
	          # if this is not the case then all subsequent lambda_cross values in the inner j-loop will be outside
	          # the hyperspace i.e. calculations are omitted
	          
	          if (j_ch==0)
	          {
	            lambda_cross<-max(0,(lambda_init*(1+magn*j/10)))
	            
	            co   <- list(REPORT=0, trace=0, maxit=1000)
	            lambda_decay<-ifelse(i+j==0,lambda_init,ifelse(j==0,
	                                                           ifelse(is.na(lambda_mat[anzsup+1+i-1,anzsup+1]),lambda_init,lambda_mat[anzsup+1+i-1,anzsup+1]),
	                                                           ifelse(is.na(lambda_mat[anzsup+1+i,anzsup+1+j-1]),lambda_init,lambda_mat[anzsup+1+i,anzsup+1+j-1])))
	            other.obj<-list(K=K,L=L,lambda=lambda,weight_func=weight_func,Lagh=Lagh,Gammah=Gammah,expweight=expweight,
	                            cutoff=cutoff,i1=i1,i2=i2,weight_constraint=weight_constraint,spec_mat=spec_mat,lambda_smooth=lambda_smooth,
	                            lambda_decay=lambda_decay,lambda_cross=lambda_cross,df_target=df_target)
	            
	            if (lambda_decay>1.e-3&lambda_decay<0.5)
	            {
	              
	              df_opt_obj <- optim(lambda_decay, df_opt,control=co, other.obj=other.obj)
	              
	              lambda_decay<-df_opt_obj$par
	              lambda_mat[anzsup+1+i,anzsup+1+j]<-lambda_decay
	              
	              i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth)
	              
	              rever_mat[anzsup+1+i,anzsup+1+j]<-i_mdfa$rever
	              df_mat[anzsup+1+i,anzsup+1+j]<-i_mdfa$degrees_freedom
	              #        print(j)
	            } else
	            {
	              print(paste("lambda_decay = ",lambda_decay,"is out of limits!!!  i=",i,", j=",j,sep=""))
	              j_ch<-1
	              # If the outerlimit is attained for j=0 then all larger i's will be out of limit too
	              if (j==0)
	                i_ch<-1
	            }
	            print(j)
	          }
	        }
	        write.table(lambda_mat,file=paste(path.out,"lambda_mat_",Lagh,sep=""))
	        write.table(rever_mat,file=paste(path.out,"rever_mat_",Lagh,sep=""))
	        write.table(df_mat,file=paste(path.out,"df_mat_",Lagh,sep=""))
	        print(c("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",i))
	      }
	    }
	    
	    i_ch<-0
	    for (i in 0:anzsup)   #i<-4
	    {
	      if (i_ch==0)
	      {
	        
	        lambda_smooth<-max(0,(lambda_init*(1+magn*i/10)))
	        j_ch<-0
	        for (j in 0:-anzsup)      #j<--anzsup
	        {
	          # it is checked whether the point in the plane is admissible i.e. whether lambda_decay has meaningful values
	          # if this is not the case then all subsequent lambda_cross values in the inner j-loop will be outside
	          # the hyperspace i.e. calculations are omitted
	          
	          if (j_ch==0)
	          {
	            lambda_cross<-max(0,(lambda_init/(1-magn*j/10)))
	            
	            co   <- list(REPORT=0, trace=0, maxit=1000)
	            lambda_decay<-ifelse(i+j==0,lambda_init,ifelse(j==0,
	                                                           ifelse(is.na(lambda_mat[anzsup+1+i-1,anzsup+1]),lambda_init,lambda_mat[anzsup+1+i-1,anzsup+1]),
	                                                           ifelse(is.na(lambda_mat[anzsup+1+i,anzsup+1+j+1]),lambda_init,lambda_mat[anzsup+1+i,anzsup+1+j+1])))
	            other.obj<-list(K=K,L=L,lambda=lambda,weight_func=weight_func,Lagh=Lagh,Gammah=Gammah,expweight=expweight,
	                            cutoff=cutoff,i1=i1,i2=i2,weight_constraint=weight_constraint,spec_mat=spec_mat,lambda_smooth=lambda_smooth,
	                            lambda_decay=lambda_decay,lambda_cross=lambda_cross,df_target=df_target)
	            
	            if (lambda_decay>1.e-3&lambda_decay<0.5)
	            {
	              
	              df_opt_obj <- optim(lambda_decay, df_opt,control=co, other.obj=other.obj)
	              
	              lambda_decay<-df_opt_obj$par
	              lambda_mat[anzsup+1+i,anzsup+1+j]<-lambda_decay
	              
	              i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth)
	              
	              rever_mat[anzsup+1+i,anzsup+1+j]<-i_mdfa$rever
	              df_mat[anzsup+1+i,anzsup+1+j]<-i_mdfa$degrees_freedom
	              #        print(j)
	            } else
	            {
	              print(paste("lambda_decay = ",lambda_decay,"is out of limits!!!  i=",i,", j=",j,sep=""))
	              j_ch<-1
	              # If the outerlimit is attained for j=0 then all susequent i's will be out of limit too: so we can drop all computations
	              if (j==0)
	                i_ch<-1
	            }
	            print(j)
	          }
	        }
	        write.table(lambda_mat,file=paste(path.out,"lambda_mat_",Lagh,sep=""))
	        write.table(rever_mat,file=paste(path.out,"rever_mat_",Lagh,sep=""))
	        write.table(df_mat,file=paste(path.out,"df_mat_",Lagh,sep=""))
	        print(c("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",i))
	      }
	    }
	    #rever_mat[anzsup+1+0:anzsup,anzsup+1+j]
	    #lambda_mat[anzsup+1+0:anzsup,anzsup+1+j]
	    #df_mat[anzsup+1+0:anzsup,anzsup+1+j]
	    i_ch<-0
	    for (i in 0:-anzsup)   #i<--10
	    {
	      if (i_ch==0)
	      {
	        
	        lambda_smooth<-max(0,(lambda_init/(1-magn*i/10)))
	        j_ch<-0
	        for (j in 0:anzsup)      #j<-0
	        {
	          # it is checked whether the point in the plane is admissible i.e. whether lambda_decay has meaningful values
	          # if this is not the case then all subsequent lambda_cross values in the inner j-loop will be outside
	          # the hyperspace i.e. calculations are omitted
	          
	          if (j_ch==0)
	          {
	            lambda_cross<-max(0,(lambda_init*(1+magn*j/10)))
	            
	            co   <- list(REPORT=0, trace=0, maxit=1000)
	            # i is negative: index i+1 in lambda_mat means the predecessor
	            lambda_decay<-ifelse(i+j==0,lambda_init,ifelse(j==0,
	                                                           ifelse(is.na(lambda_mat[anzsup+1+i+1,anzsup+1]),lambda_init,lambda_mat[anzsup+1+i+1,anzsup+1]),
	                                                           ifelse(is.na(lambda_mat[anzsup+1+i,anzsup+1+j-1]),lambda_init,lambda_mat[anzsup+1+i,anzsup+1+j-1])))
	            other.obj<-list(K=K,L=L,lambda=lambda,weight_func=weight_func,Lagh=Lagh,Gammah=Gammah,expweight=expweight,
	                            cutoff=cutoff,i1=i1,i2=i2,weight_constraint=weight_constraint,spec_mat=spec_mat,lambda_smooth=lambda_smooth,
	                            lambda_decay=lambda_decay,lambda_cross=lambda_cross,df_target=df_target)
	            
	            if (lambda_decay>1.e-3&lambda_decay<0.5)
	            {
	              
	              df_opt_obj <- optim(lambda_decay, df_opt,control=co, other.obj=other.obj)
	              
	              lambda_decay<-df_opt_obj$par
	              lambda_mat[anzsup+1+i,anzsup+1+j]<-lambda_decay
	              
	              i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth)
	              
	              rever_mat[anzsup+1+i,anzsup+1+j]<-i_mdfa$rever
	              df_mat[anzsup+1+i,anzsup+1+j]<-i_mdfa$degrees_freedom
	              #        print(j)
	            } else
	            {
	              print(paste("lambda_decay = ",lambda_decay,"is out of limits!!!  i=",i,", j=",j,sep=""))
	              j_ch<-1
	              # If the outerlimit is attained for j=0 then all susequent i's will be out of limit too: so we can drop all computations
	              if (j==0)
	                i_ch<-1
	            }
	            print(j)
	          }
	        }
	        write.table(lambda_mat,file=paste(path.out,"lambda_mat_",Lagh,sep=""))
	        write.table(rever_mat,file=paste(path.out,"rever_mat_",Lagh,sep=""))
	        write.table(df_mat,file=paste(path.out,"df_mat_",Lagh,sep=""))
	        print(c("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",i))
	      }
	    }
	    
	    
	    
	    
	    i_ch<-0
	    for (i in 0:-anzsup)   #i<--anzsup
	    {
	      if (i_ch==0)
	      {
	        lambda_smooth<-max(0,(lambda_init/(1-magn*i/10)))
	        j_ch<-0
	        for (j in 0:-anzsup)      #j<-3
	        {
	          # it is checked whether the point in the plane is admissible i.e. whether lambda_decay has meaningful values
	          # if this is not the case then all subsequent lambda_cross values in the inner j-loop will be outside
	          # the hyperspace i.e. calculations are omitted
	          
	          if (j_ch==0)
	          {
	            lambda_cross<-max(0,(lambda_init/(1-magn*j/10)))
	            
	            co   <- list(REPORT=0, trace=0, maxit=1000)
	            # i is negative: index i+1 in lambda_mat means the predecessor
	            lambda_decay<-ifelse(i+j==0,lambda_init,ifelse(j==0,
	                                                           ifelse(is.na(lambda_mat[anzsup+1+i+1,anzsup+1]),lambda_init,lambda_mat[anzsup+1+i+1,anzsup+1]),
	                                                           ifelse(is.na(lambda_mat[anzsup+1+i,anzsup+1+j+1]),lambda_init,lambda_mat[anzsup+1+i,anzsup+1+j+1])))
	            other.obj<-list(K=K,L=L,lambda=lambda,weight_func=weight_func,Lagh=Lagh,Gammah=Gammah,expweight=expweight,
	                            cutoff=cutoff,i1=i1,i2=i2,weight_constraint=weight_constraint,spec_mat=spec_mat,lambda_smooth=lambda_smooth,
	                            lambda_decay=lambda_decay,lambda_cross=lambda_cross,df_target=df_target)
	            
	            if (lambda_decay>1.e-3&lambda_decay<0.5)
	            {
	              
	              df_opt_obj <- optim(lambda_decay, df_opt,control=co, other.obj=other.obj)
	              
	              lambda_decay<-df_opt_obj$par
	              lambda_mat[anzsup+1+i,anzsup+1+j]<-lambda_decay
	              
	              i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth)
	              
	              rever_mat[anzsup+1+i,anzsup+1+j]<-i_mdfa$rever
	              df_mat[anzsup+1+i,anzsup+1+j]<-i_mdfa$degrees_freedom
	              #        print(j)
	            } else
	            {
	              print(paste("lambda_decay = ",lambda_decay,"is out of limits!!!  i=",i,", j=",j,sep=""))
	              j_ch<-1
	              # If the outerlimit is attained for j=0 then all susequent i's will be out of limit too: so we can drop all computations
	              if (j==0)
	                i_ch<-1
	            }
	            print(j)
	          }
	        }
	        write.table(lambda_mat,file=paste(path.out,"lambda_mat_",Lagh,sep=""))
	        write.table(rever_mat,file=paste(path.out,"rever_mat_",Lagh,sep=""))
	        write.table(df_mat,file=paste(path.out,"df_mat_",Lagh,sep=""))
	        print(c("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",i))
	      }
	    }
	    
	    
	    return(list(lambda_mat=lambda_mat,rever_mat=rever_mat,df_mat=df_mat,lambda_init=lambda_init))
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  df_init<-function(lambda_init,other.obj)
	  {
	    K<-other.obj$K
	    L<-other.obj$L
	    lambda<-other.obj$lambda
	    weight_func<-other.obj$weight_func
	    Lagh<-other.obj$Lagh
	    Gammah<-other.obj$Gammah
	    expweight<-other.obj$expweight
	    cutoff<-other.obj$cutoff
	    i1<-other.obj$i1
	    i2<-other.obj$i2
	    weight_constraint<-other.obj$weight_constraint
	    spec_mat<-other.obj$spec_mat
	    df_target<-other.obj$df_target
	    
	    lambda_smooth<-max(0.000001,abs(lambda_init))
	    lambda_cross<-lambda_smooth
	    lambda_decay<-lambda_smooth
	    i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,
	                              lambda_cross,lambda_decay,lambda_smooth)
	    
	    # in order to avoid unnecessary precision tuning the criterion is set to one if the error between df_target and
	    # the degerees of freedom is smaller than 0.5 in absolute value
	    return(ifelse(abs(K-i_mdfa$degrees_freedom-df_target)>0.5,abs(K-i_mdfa$degrees_freedom-df_target),0))
	  }
	  
	  
	  
	  
	  df_opt<-function(lambda_opt,other.obj)
	  {
	    K<-other.obj$K
	    L<-other.obj$L
	    lambda<-other.obj$lambda
	    weight_func<-other.obj$weight_func
	    Lagh<-other.obj$Lagh
	    Gammah<-other.obj$Gammah
	    expweight<-other.obj$expweight
	    cutoff<-other.obj$cutoff
	    i1<-other.obj$i1
	    i2<-other.obj$i2
	    weight_constraint<-other.obj$weight_constraint
	    spec_mat<-other.obj$spec_mat
	    df_target<-other.obj$df_target
	    lambda_smooth<-other.obj$lambda_smooth
	    lambda_cross<-other.obj$lambda_cross
	    lambda_decay<-max(0.0001,abs(lambda_opt))
	    
	    i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth)
	    
	    # in order to avoid unnecessary precision tuning the criterion is set to one if the error between df_target and
	    # the degerees of freedom is smaller than 0.5 in absolute value
	    return(ifelse(abs(K-i_mdfa$degrees_freedom-df_target)>0.5,abs(K-i_mdfa$degrees_freedom-df_target),0))
	  }
	  
	  
	  
	  
	  
	  aic_reg_draw<-function(i1,i2,expweight,lambda,L,spec_mat,l_s_vec,l_c_vec,l_l_vec,K,weight_func,Lagh,Gammah,cutoff,weight_constraint,lambda_decay,x,x_sym,len,cli,aic_array,minint)
	  {
	    
	    minaic<-as.vector(aic_array)[order(aic_array)[minint]]
	    i_check=0
	    for (i in 1:length(l_s_vec))
	    {
	      if (length(which(aic_array[i,,]==minaic))>0)
	      {
	        i_min<-i
	        i_check=1
	      }
	    }
	    if (i_check==0)
	    {
	      print("No minimum found: minint too large!!!!")
	    } else
	    {
	      for (j in 1:length(l_c_vec))
	      {
	        if (length(which(aic_array[,j,]==minaic))>0)
	          j_min<-j
	      }
	      for (k in 1:length(l_l_vec))
	      {
	        if (length(which(aic_array[,,k]==minaic))>0)
	          k_min<-k
	      }                                            # i_min<-51  j_min<-1   k_min<-21              x11()
	      lambda_smooth<-l_s_vec[i_min]
	      lambda_cross<-l_c_vec[j_min]
	      lambda_decay<-l_l_vec[k_min]
	      
	      i_mdfa<-mdfa_analytic_new(K,L,lambda,weight_func,Lagh,Gammah,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth)
	      
	      print(i_mdfa$degrees_freedom)
	      print(i_mdfa$aic)
	      
	      par(mfrow=c(3,1))
	      ts.plot(abs(i_mdfa$trffkt))
	      ts.plot(i_mdfa$b)
	      
	      xff<-x[,-1]*0
	      for (i in L:len) #i<-20
	      {
	        xff[i,]<-0
	        for (j in 2:(length(x[1,])))  #j<-2
	        {
	          xff[i,j-1]<-xff[i,j-1]+i_mdfa$b[,j-1]%*%x[i:(i-L+1),j]
	        }
	      }
	      
	      ts.plot(scale(cbind(apply(xff,1,sum),x_sym[,1],cli)),lty=1:3)
	      return(list(xff=xff))
	    }
	  }
	  # Copyright: Marc Wildi
	  # 24.02.2012
	  # http://blog.zhaw.ch/idp/sefblog
	  
	  
	  
	  
	  
	  
	  
	  plot_in_out<-function(x,x_sym,xff,imdfa)
	  {
	    i_mdfa$degrees_freedom
	    
	    colo<-rainbow(length(i_mdfa$b[1,]))
	    par(mfrow=c(2,1))
	    mplot<-abs(i_mdfa$trffkt)
	    plot_title<-paste("Lag ",Lag," I-MDFA: from ",xdate[1]," to ", xdate[insamp],sep="")
	    title_more<-dimnames(i_mdfa$trffkt)[[2]]
	    mplot<-abs(i_mdfa$trffkt)
	    freq_axe<-rep(NA,K+1)
	    freq_axe[1]<-0
	    freq_axe[1+(1:6)*K/6]<-c(paste(c("",2:5),"pi/6",sep=""),"pi")
	    mplot_func(mplot,freq_axe,plot_title,title_more)
	    plot_title<-"Filter coefficients"
	    mplot<-i_mdfa$b
	    mplot_func(mplot,0:(L-1),plot_title,title_more)
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  #ax<-freq_axe
	  #insamp<-1.e+90
	  
	  
	  mplot_func<-function(mplot,ax,plot_title,title_more,insamp,color,freq_scale)
	  {
	    lwde<-1
	    cexe<-0.9
	    anf<-1
	    len<-length(mplot[,1])
	    # If signal forecasts are computed then revision matrix is longer than data matrix
	    #    mplot<-scale(mplot)
	    ymin=min(mplot,na.rm=T)
	    ymax=max(mplot,na.rm=T)
	    if (color)
	    {
	      colo<-rainbow(dim(mplot)[2])
	      lty_vec<-rep(1,dim(mplot)[2])
	    } else
	    {
	      colo<-rep("black",dim(mplot)[2])
	      lty_vec<-1:dim(mplot)[2]
	    }
	    plot(mplot[,1],ylim=c(ymin,ymax),type="l",
	         axes=F,col="black",main=plot_title,xlab="",ylab="",lwd=lwde,cex=cexe,lty=lty_vec[1])
	    if (!(insamp==1.e+90))
	    {
	      abline(v=insamp)
	      text(x=insamp,y=-2,"End of In-Sample Window")  
	    }
	    abline(h=0)
	    if (length(title_more)>0)
	      mtext(title_more[1],col="black",cex=cexe)
	    if (dim(mplot)[2]>1)
	    {
	      for (j in 2:dim(mplot)[2])
	      {
	        lines(mplot[,j],col=colo[j],lwd=lwde,lty=lty_vec[j])
	        if (length(title_more)>0)
	          mtext(title_more[j],col=colo[j],line=-(j-1),cex=cexe)
	      }
	    }
	    if (!freq_scale)
	    {
	      #    axis(1,at=1:len,labels=ax)
	      axis(1,at=which(!is.na(ax)),labels=ax[!is.na(ax)])
	    } else
	    {
	      axis(1,at=c(1,(1:6)*(len/6)),labels=c(0,"pi/6","2pi/6","3pi/6","4pi/6","5pi/6","pi"))
	    }
	    axis(2)
	    box()
	    
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  mplot_func_recession<-function(mplot,ax,plot_title,title_more,insamp,rec_dates,color)
	  {
	    #March2001-Nov2001
	    #Dec2007-June2009
	    lwde<-1
	    cexe<-1
	    anf<-1
	    len<-length(mplot[,1])
	    # If signal forecasts are computed then revision matrix is longer than data matrix
	    #    mplot<-scale(mplot)
	    ymin=min(mplot,na.rm=T)
	    ymax=max(mplot,na.rm=T)
	    if (color)
	    {
	      colo<-rainbow(dim(mplot)[2])
	      lty_vec<-rep(1,dim(mplot)[2])
	    } else
	    {
	      colo<-rep("black",dim(mplot)[2])
	      lty_vec<-1:dim(mplot)[2]
	    }
	    
	    plot(mplot[,1],ylim=c(ymin,ymax),type="l",
	         axes=F,col="black",main=plot_title,xlab="",ylab="",lwd=lwde,cex=cexe,lty=lty_vec[1])
	    usrh <- par('usr')
	    if (!(insamp==1.e+90))
	    {
	      abline(v=insamp)
	      text(x=insamp,y=-2,"End of In-Sample Window")  
	    }
	    abline(h=0)
	    if (length(title_more)>0)
	      mtext(title_more[1],col="black",cex=cexe)
	    if (dim(mplot)[2]>1)
	    {
	      for (j in 2:dim(mplot)[2])
	      {
	        lines(mplot[,j],col=colo[j],lwd=lwde,lty=lty_vec[j])
	        if (length(title_more)>0)
	          mtext(title_more[j],col=colo[j],line=-(j-1),cex=cexe)
	      }
	    }
	    for (i in 1:(length(rec_dates)/2)) #i<-1
	    {
	      st_i<-which(ax==rec_dates[(i-1)*2+1])
	      if (length(st_i)>0)
	      {
	        e_i<-which(ax==rec_dates[(i-1)*2+2])
	        rect(st_i, usrh[3], e_i, usrh[4], col='grey',density=10)      
	      }
	    }
	    axis(1,at=c(1,1+(1:(len/12))*12),labels=ax[c(1,1+(1:(len/12))*12)])
	    axis(2)
	    box()
	    
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  trans_func<-function(b,l)
	  {
	    trffkt<-0:(l-1)
	    for (i in 0:(l-1))
	    {
	      omegak<-i*pi/(l-1)
	      trffkt[i+1]<-b%*%exp(1.i*omegak*(0:(length(b)-1)))
	    }
	    return(list(trffkt=trffkt))
	  }
	  
	  
	  
	  print_func<-function(GDP_T,trend_T,lb,ub)
	  {
	    
	    
	    if (GDP_T)
	    { 
	      print("GDP is targeted")
	    } else
	    {
	      print("`Coin is targeted")
	    }  
	    if (trend_T)
	    {
	      print(paste("Trend signal with cutoff ",2*ub," months",sep=""))
	    } else
	    {
	      print(paste("Bandpass signal with cutoffs ",2*ub," - ",2*lb," months",sep=""))  
	    }
	    
	    print("Please go on with file regularization.r")
	    
	    
	  }
	  
	  
	  
	  
	  
	  
	  
	  
	  
	  load.packages('quantmod,quadprog,corpcor,lpSolve,corrplot,xts,lubridate,jsonlite')
	  
	  str2expr<-function(x){eval(parse(text=x), envir=parent.frame() )} 
	  trim <- function (x)  sub("^\\s+", "", x)
	  
	  delay <- function(x,k) {
	    result <- lag(x,k); 
	    return(result)
	  }
	  
	  sma <- function(x,n) {
	    result <- SMA(x,n); 
	    return(result)
	  }
	  
	  less_or_equal <- function(x,y) {
	    result <- ifelse(x <= y,1,0);
	    return(result)
	  }
	  
	  
	  greater_or_equal <- function(x,y) {
	    result <- ifelse(x >= y,1,0);
	    return(result)
	  }
	  
	  or <- function(x,y) {
	    result <- ifelse(x > 0|y > 0,1,0);
	    return(result)
	  }
	  
	  
	  and <- function(x,y) {
	    result <- ifelse(x > 0&y > 0,1,0);
	    return(result)
	  }
	  
	  sgn <- function(x) {
	    result <- ifelse(x>0,1,ifelse(x<0,-1,0));
	    return(result)
	  }
	  
	  mod <- function(x,m)
	  {
	    t1<-floor(x/m)
	    return(x-t1*m)
	  }
	  
	  xor <- function(x,y,z) {
	    result <- ifelse(x <= 0 & y > 0,1,0);
	    result <- ifelse(x > 0 & y <= 0,1,result);
	    return(result)
	  }
	  
	  not <- function(x) {
	    result <- ifelse(x > 0,0,1);
	    return(result)
	  }
	  
	  equal <- function(x,y) {
	    result <- ifelse(x == y,1,0);
	    return(result)
	  }
	  
	  load.barchart<- function(symbol=NULL){
	    test <- sprintf("http://marketdata.websol.barchart.com/getHistory.json?key=6ee936525f99e04faf7f9b0b4a1c5011&symbol=%s&type=daily&startDate=20110101", symbol)
	    gdxjson <- fromJSON(test)
	    #test <- sprintf("http://marketdata.websol.barchart.com/getQuote.json?key=6ee936525f99e04faf7f9b0b4a1c5011&symbols=%s", symbol)
	    test <- sprintf("http://barchartjson.websol.barchart.com/?module=jsonQuotes&symbol=%s", symbol)
	    gdxjson2 <- fromJSON(test)
	    history <- gdxjson$results
	    quote <- gdxjson2[1,]
	    test = nrow(history)+1
	    latest <- history[nrow(history),]
	    latest$close <- as.numeric(quote$last)
	    latest$open <- as.numeric(quote$open)
	    latest$high <- as.numeric(quote$high)
	    latest$low <- as.numeric(quote$low)
	    latest$volume <- as.numeric(gsub(",","",quote$volume))
	    latest$tradingDay <- as.character(today())
	    if (latest$tradingDay != tail(history$tradingDay,1)) { history[test,] <- latest[1,]}
	    history <- history[,-1:-2]
	    rownames(history) <- history$tradingDay
	    history$tradingDay <- NULL
	    history$openInterest <- NULL
	    out <- as.xts(history)
	    return( out )
	  }
	  
	  
	  load.barchart.intraday<- function(symbol=NULL,interval=NULL){
	    test <- sprintf("http://marketdata.websol.barchart.com/getHistory.json?key=6ee936525f99e04faf7f9b0b4a1c5011&symbol=%s&type=minutes&interval=%s&startDate=20100101", symbol,interval)
	    gdxjson <- fromJSON(test)
	    history <- gdxjson$results
	    history$Date <- substr(history$timestamp,1,19)
	    history$Date <- gsub("T", " ", history$Date)
	    history$timestamp <- NULL
	    history$tradingDay <- NULL
	    history$symbol <- NULL
	    history$openInterest <- NULL
	    history <- history[,c(6,1,2,3,4,5)]
	    dat <- read.zoo(history,tz ='' , format = "%Y-%m-%d %H:%M:%S",header=TRUE,
	                    sep='')
	    out<-as.xts(dat)
	    return(out)
	  }
	  
	  library(quantmod)
		data = getData()
		
	tryCatch({							
		#*****************************************************************
		# Code Strategies
		#******************************************************************
		stock <- getStock() 
		x	 <- as.data.frame(getData()$prices)
		x <- ROC(x)
		x[1:3,]=0
		if(ncol(x)< 2) {
		  x[,2]<-x[,1]
		}
		xb <- x
		test <- as.integer(input$oos1)+as.integer(input$oos)
		x <-x[as.integer(input$oos1):test,]
	
		
		len<-length(x[,1])
			####
		### IDMFA paramters
		###
		ub<-as.double(input$ub)
		lb<-as.double(input$lb)
		cutoff<-pi/ub
		Lag<-as.double(input$lag)
		lin_expweight<-F
		weight_constraint<-rep(1/(length(x[1,])-1),length(x[1,])-1)
		d<-0
		spec_obj<-spec_comp(len,x,d)
		weight_func<-spec_obj$weight_func
		K<-length(weight_func[,1])-1
	  Gamma<-((0:K)<(K*ub))
	  #Gamma<-((0:K)<K/ub)&((0:K)>K/lb)
		L<-as.double(input$smaLen)
		
		lambda_smooth<-as.double(input$overfit)/10
		lambda_decay<-as.double(input$overfit)/100
		lambda<-0
		expweight<-as.integer(input$smooth)
		lambda_cross<-0
		i1<-F
		i2<-F
	
		
		plots=T
	  i_mdfa_obj<-IMDFA_comp(Lag,K,L,lambda,weight_func,Gamma,expweight,cutoff,i1,i2,weight_constraint,lambda_cross,lambda_decay,lambda_smooth,x,plots,lin_expweight)
	  
	  b<-i_mdfa_obj$i_mdfa$b
   
	  #prices$signal <- i_mdfa_obj$xff
	  L2<-length(b)
	  len2 <- nrow(xb)
	  xf<-NULL
	  for (i in L2:len2) #i<-20
	  {
	    # I-MDFA
	    xf[i]<-0
	    for (j in 2:length(xb[1,]))  #j<-2
	      xf[i]<-xf[i]+b[,j-1]%*%xb[i:(i-L2+1),j]
	  }
	 	#*****************************************************************
	  # Buy & Hold
	  #****************************************************************** 
	  models = list()
	  data$weight[] = NA
	  data$weight[,stock] = 1
	  models$buy.hold = bt.run.share(data, clean.signal=T)
		#*****************************************************************
		# MA cross-over strategy
		#****************************************************************** 

	  data$weight[] = NA
    trigger <- ifelse(xf<(-0.00),-1,ifelse(xf>0.000,1,0))
		data$weight[,stock] = trigger
		models$filter = bt.run.share(data, clean.signal=F, trade.summary = T)
	
		models$filter$b <- b
		rev(models)
	}, error = function(e) { stop(paste('Problem running Back Test:', e)) })
	})
	
		
	# Make table
	makeSidebysideTable <- reactive(function() {
		models = getBackTest()
		plotbt.strategy.sidebyside(models, return.table=T, make.plot=F)
	})

	# Make table
	makeAnnualTable <- reactive(function() {
		models = getBackTest()
		plotbt.monthly.table(models[[1]]$equity, make.plot = F)
	})
	
	# Make table
	makeTradesTable <- reactive(function() {
		models = getBackTest()
		model = models[[1]]
		
		if (!is.null(model$trade.summary)) {
			ntrades = min(20, nrow(model$trade.summary$trades))		
			last(model$trade.summary$trades, ntrades)
		}
	})
		
	# Generate a table
	makefilterTable <- reactive(function() {
	  b = getBackTest()$filter 
	  b
	})
    #*****************************************************************
    # Not Reactive helper functions
	#*****************************************************************
	
	
    #*****************************************************************
    # Update plot(s) and table(s)
    #******************************************************************    	
	# Generate a plot
	output$strategyPlot <- reactivePlot(function() {
		models = getBackTest()
		models$filter$weight2 <- models$filter$weight
		test2 <- as.integer(input$oos1)+as.integer(input$oos)
		models$filter$weight2[input$oos1:test2] <- 3
		highlight = which(models$filter$weight2 > 2)
		plota.control$col.x.highlight = col.add.alpha('green',50)
			plotbt(models, plotX = T, log = 'y', LeftMargin = 3, main = NULL,x.highlight = highlight)
		
		
		#plotbt.custom.report.part1(models, x.highlight = models$filter$highlight)  
	}, height = 600, width = 900)
	
	# Generate a table
  	output$sidebysideTable <- reactivePlot(function() {
		#temp = makeSidebysideTable()	
		#tableColor(as.matrix(temp))	
  	  models = getBackTest()
		plotbt.custom.report.part2(models)  
	}, height = 600, width = 900)
	
	# Generate a table
  	output$annualTable <- reactive(function() {
		temp = makeAnnualTable()	
		tableColor(as.matrix(temp))		
	})

	# Generate a plot
	output$transitionPlot <- reactivePlot(function() {
		models = getBackTest()
		plotbt.transition.map(models[[1]]$weight)	
	}, height = 400, width = 600)
		
	# Generate a table
  	output$tradesTable <- reactive(function() {
		temp = makeTradesTable()	
		tableColor(as.matrix(temp))		
	})
  
	
    #*****************************************************************
    # Download
    #******************************************************************    
    # Download pdf report
	output$downloadReport <- downloadHandler(
    	filename = 'report.pdf',
    	content = function(file) {
    		pdf(file = file, width=8.5, height=11)
      			
    		models = getBackTest()
    		
    		plota.control$col.x.highlight = col.add.alpha('green',50)
    		plotbt.custom.report(models, trade.summary = T, x.highlight = models$market.filter$highlight)
				plota.add.copyright()
      		
		    dev.off()
    	}
	)	
		
	 output$downloadData <- downloadHandler(
	filename = function() {
	  paste('filter-', Sys.Date(), '.csv', sep='')
   },
	  content = function(file) {
	    write.csv(makefilterTable()$b, file,col.names = FALSE,row.names = FALSE)
  }
  )
	 
	
    #*****************************************************************
    # Update status message 
    #******************************************************************    
	output$status <- reactiveUI(function() {
		out = tryCatch( getData(), error=function( err ) paste(err))	    				
		if( is.character( out ) ) 
			HTML(paste("<b>Status</b>: <b><font color='red'>Error:</font></b>",out))
		else
			HTML("<b>Status</b>: <b><font color='green'>Ok</font></b>")		
	})
	
	
})
