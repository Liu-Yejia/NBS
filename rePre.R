load(file = "/Users/liuyejia_2021/Downloads/rePre.rdata")

Z_extreme_2022_binary<-function(x, amount=40){
  # import packages
  if(1){
    # library(rpart)
    # library(kernlab)
    library(randomForest)
    # library(adabag)
    # library(HDclassif)
    # library(RWeka)
    # library(xgboost)
    # library(glmnet)
    library(sdwd)
    library(beepr)
    library(matrixStats)
    library(stringr)
  }
  # define several self-made functions
  if(1){
    is_odd<-function(x){
      if(x%%2==1){
        return(1)
      }else{
        return(0)
      }
    }# judge if the input is odd
    proportion<-function(x){
      return(x/rowSums(x))
    }# convert input to frequency
    cut_out<-function(i, length = 40){
      # extract a vector of a continuous integers of 'i'th, and the length is given by parameter 'length'
      return(c((i-1)*length+1):(i*length))
    }# specify the row numbers for a extreme setup
    # 任意の一つの行ベクトル（仮にアとする）に対して、二種類の学習データの番号を生成する、すなわち：
    #（１）アを書いた著者（a）のア以外の同ジャンル（A）の作品ともう一人の著者（b）の異なるジャンル（B）の作品からなる学習データ
    #（２）著者（a）のジャンル（B）の作品と著者（b）のジャンル（A）の作品からなる学習データ
    section<-function(i){
      # section is a workflow to decide the numbers for training for each iteration, 
      # and the output of section are two vectors containing the numbers for two situations 
      cuneiform<-(i-1)%/%20+1# 楔(所属のブロック;界定所属区块)
      train_case_1_num<-c(cut_out(i=cuneiform, length=20), cut_out(i=(5-cuneiform), length=20))
      position<-which(train_case_1_num==i)
      train_case_1_num<-train_case_1_num[-position]
      if(cuneiform<=2){
        second<-3-cuneiform
        third<-5-second
      }else{
        second<-7-cuneiform
        third<-5-second
      }
      train_case_2_num<-c(cut_out(i=second, length=20), cut_out(i=third, length=20))
      list(case_1=train_case_1_num, case_2=train_case_2_num)
    }
    # 予測結果を縦に収納するマトリックスの各列と、与えられたもう一つの文字列を要素とするベクトル、すなわち、本当の著者ラベルとのマッチングを行い、
    # リスト形式で二つの要素を結果として出力する、それぞれ、正解率とF1値
    operator_new<-function(x, lab){
      len<-ncol(x)
      accu<-vector(length=len)
      len_lab<-length(lab)
      for(i in 1:len){
        accu[i]<-sum(x[, i]==lab)/len_lab
      }
      return(accu)
    }
    shorten<-function(x, threshold=0){
      if(threshold==0){
        return(x)
      }else{
        n<-which(colSums(x)>=threshold)
        if(ncol(x)-length(n)>=2){
          return(cbind(x[, n], Others=rowSums(x[, -n])))
        }else{
          return(x)
        }
      }
    }
    shorten_new<-function(x, threshold=0, discard=0.05){
      if(max(x[1,])<1){#检测x是否为相对度数矩阵
        if(discard==0){
          return(x)
        }else{
          cS<-colSums(x)# 求各列和
          cS_increase<-sort(unique(cS), decreasing=FALSE)# 按升序排列cS后去重
          need2Bdiscarded_values<-cS_increase[1:ceiling(length(cS_increase)*discard)]# 取最小的discard%的值
          need2Bdiscarded_sn<-NULL
          for(i in need2Bdiscarded_values){
            need2Bdiscarded_sn<-c(need2Bdiscarded_sn, which(cS==i))
          }
          return(cbind(x[, -need2Bdiscarded_sn], Others=rowSums(x[, need2Bdiscarded_sn])))
        }
      }else{
        if(threshold==0){
          return(x)
        }else{
          n<-which(colSums(x)>=threshold)
          return(cbind(x[,n], Others=rowSums(x[,-n])))
        }
      }
    }# 含两套次元削减方法：1. 百分比；2. 阈值；优先百分比法
    replacement<-function(x){
      len<-length(x)
      factors_names<-unique(x)
      factors_num<-length(factors_names)
      res<-vector(length=len)
      for(i in 1:len){
        res[i]<-which(factors_names==x[i])-1
      }
      return(res)
    }# 输入为以文字为元素的向量，按输入顺序将文字元素转换为数字，初值为0，等差为1
    twice<-function(x, times=2){
      result<-x
      for(i in 1:(times-1)){
        result<-rbind(result, x)
      }
      return(result)
    }# 同じベクトルを指定した回数横に重ねる
  }
  # test environment
  if(0){
    i<-6
    j<-60
    amount<-40
  }
  # set-up
  if(1){
    true_cn<-colnames(x)
    dummy_cn<-paste0("V", 1:length(true_cn))
    colnames(x)<-dummy_cn
    author_vector<-c("岩井俊二", "貴志祐介", "吉田修一", "宮部みゆき", "森見登美彦", "石田衣良", "村上春樹", "村上龍", 　"東野圭吾", 　"湊かなえ", "鈴木光司")
    enumeration<-t(combn(c(1:11), m=2))
    enumeration_num<-nrow(enumeration)
    corresponding_authors_sets<-matrix(author_vector[enumeration], enumeration_num, 2)
    last_accu_A<-matrix(0, enumeration_num, 2)
    dimnames(last_accu_A)<-list(Sets=str_c(corresponding_authors_sets[, 1], " & ", corresponding_authors_sets[, 2]),
                              # Classifiers=c("svm", "rf", "hdda", "lmt", "xgboost", "lasso", "sdwd"))
                                Classifiers=c("rf", "sdwd"))
    last_accu_B<-matrix(0, enumeration_num, 2)
    dimnames(last_accu_B)<-list(Sets=str_c(corresponding_authors_sets[, 1], " & ", corresponding_authors_sets[, 2]),
                              # Classifiers=c("svm", "rf", "hdda", "lmt", "xgboost", "lasso", "sdwd"))
                                Classifiers=c("rf", "sdwd"))
  }
  # begin to iterate
  for(i in 1:enumeration_num){
    # set-up
    if(1){
      a_all<-cut_out(enumeration[i,1])
      b_all<-cut_out(enumeration[i,2])
      a_A<-a_all[1:20]; a_B<-a_all[21:40]
      b_A<-b_all[1:20]; b_B<-b_all[21:40]
      slice_number<-c(a_A, b_A, a_B, b_B)
      unique_authors<-author_vector[enumeration[i,]]
      label<-as.factor(rep(rep(unique_authors, each=20), 2))
      current_data<-x[slice_number, ]
      if(0){
        if(max(current_data[1, ])<1){
          slice<-shorten_new(current_data, discard=discard)
        }else{
          if(current_data|>colSums()|>sort(dec=T)|>head(5)|>tail(1)>=10){
            slice<-proportion(shorten(current_data, threshold=10))
          }else{
            new_thre<-c(current_data|>colSums()|>sort(dec=T)|>head(5)|>tail(1)/2, 2)|>max()
            slice<-proportion(shorten(current_data, threshold=new_thre))
          }
        }
      }
      if(1){
        slice<-current_data[, which(colSums(current_data)>=2)]
      }
      # slice_for_LMT<-data.frame(slice, labels=label)
      res_case_1<-matrix(0, 80, 2)# for genre A
      res_case_2<-matrix(0, 80, 2)# for genre B
      # 由于case_2的行编号与case_1相比相对固定：指向当前作品的指针移动20个单位，case_2的行编号才发生一次变化。故对于每一对作家的作品子集，在指针移动开始前建立4组模型，避免19*4=76次重复建模。
      case_2_I  <- section(1)[[2]]
      case_2_II <-section(21)[[2]]
      case_2_III<-section(41)[[2]]
      case_2_IV <-section(61)[[2]]
      # 针对case2，建立一个双层list对象，用以存储上述4组行编号各自对应的预测模型
      if(1){
        models_repos<-list()
        if(1){
        # case.2_I_model.svm<-ksvm(x=as.matrix(slice[case_2_I,]), y=label[case_2_I], scaled=FALSE)
          case.2_I_model.rf<-randomForest(x=slice[case_2_I,], y=label[case_2_I])
        # case.2_I_model.adaboost<-boosting(labels~., data=slice_for_LMT[case_2_I,])
        # case.2_I_model.hdda<-hdda(data=slice[case_2_I,], cls=label[case_2_I], model=14)
        # case.2_I_model.lmt<-LMT(labels~., data=slice_for_LMT[case_2_I,])
        # case.2_I_model.xgboost<-xgboost(data=as.matrix(slice[case_2_I,]), max_depth=6, label=replacement(label[case_2_I]), objective="multi:softmax", num_class=2, nrounds=2, eta=0.3, eval_metric="merror")
        # case.2_I_model.lasso<-glmnet(x=slice[case_2_I,], y=label[case_2_I], family="binomial")
          case.2_I_model.sdwd<-sdwd(x=slice[case_2_I,], y=label[case_2_I], lambda2=1)
          models_repos_I<-list(# case.2_I_model.svm,
                               case.2_I_model.rf,
                             # case.2_I_model.adaboost,
                             # case.2_I_model.hdda,
                             # case.2_I_model.lmt,
                             # case.2_I_model.xgboost,
                             # case.2_I_model.lasso,
                               case.2_I_model.sdwd)
          models_repos[[1]]<-models_repos_I
        }# for case_2_I
        if(1){
        # case.2_II_model.svm<-ksvm(x=as.matrix(slice[case_2_II,]), y=label[case_2_II], scaled=FALSE)
          case.2_II_model.rf<-randomForest(x=slice[case_2_II,], y=label[case_2_II])
        # case.2_II_model.adaboost<-boosting(labels~., data=slice_for_LMT[case_2_II,])
        # case.2_II_model.hdda<-hdda(data=slice[case_2_II,], cls=label[case_2_II], model=14)
        # case.2_II_model.lmt<-LMT(labels~., data=slice_for_LMT[case_2_II,])
        # case.2_II_model.xgboost<-xgboost(data=as.matrix(slice[case_2_II,]), max_depth=6, label=replacement(label[case_2_II]), objective="multi:softmax", num_class=2, nrounds=2, eta=0.3, eval_metric="merror")
        # case.2_II_model.lasso<-glmnet(x=slice[case_2_II,], y=label[case_2_II], family="binomial")
          case.2_II_model.sdwd<-sdwd(x=slice[case_2_II,], y=label[case_2_II], lambda2=1)
          models_repos_II<-list(# case.2_II_model.svm,
                                case.2_II_model.rf,
                              # case.2_II_model.adaboost,
                              # case.2_II_model.hdda,
                              # case.2_II_model.lmt,
                              # case.2_II_model.xgboost,
                              # case.2_II_model.lasso,
                                case.2_II_model.sdwd)
          models_repos[[2]]<-models_repos_II
        }# for case_2_II
        if(1){
        # case.2_III_model.svm<-ksvm(x=as.matrix(slice[case_2_III,]), y=label[case_2_III], scaled=FALSE)
          case.2_III_model.rf<-randomForest(x=slice[case_2_III,], y=label[case_2_III])
        # case.2_III_model.adaboost<-boosting(labels~., data=slice_for_LMT[case_2_III,])
        # case.2_III_model.hdda<-hdda(data=slice[case_2_III,], cls=label[case_2_III], model=14)
        # case.2_III_model.lmt<-LMT(labels~., data=slice_for_LMT[case_2_III,])
        # case.2_III_model.xgboost<-xgboost(data=as.matrix(slice[case_2_III,]), max_depth=6, label=replacement(label[case_2_III]), objective="multi:softmax", num_class=2, nrounds=2, eta=0.3, eval_metric="merror")
        # case.2_III_model.lasso<-glmnet(x=slice[case_2_III,], y=label[case_2_III], family="binomial")
          case.2_III_model.sdwd<-sdwd(x=slice[case_2_III,], y=label[case_2_III], lambda2=1)
          models_repos_III<-list(# case.2_III_model.svm,
                                 case.2_III_model.rf,
                               # case.2_III_model.adaboost,
                               # case.2_III_model.hdda,
                               # case.2_III_model.lmt,
                               # case.2_III_model.xgboost,
                               # case.2_III_model.lasso,
                                 case.2_III_model.sdwd)
          models_repos[[3]]<-models_repos_III
        }# for case_2_III
        if(1){
        # case.2_IV_model.svm<-ksvm(x=as.matrix(slice[case_2_IV,]), y=label[case_2_IV], scaled=FALSE)
          case.2_IV_model.rf<-randomForest(x=slice[case_2_IV,], y=label[case_2_IV])
        # case.2_IV_model.adaboost<-boosting(labels~., data=slice_for_LMT[case_2_IV,])
        # case.2_IV_model.hdda<-hdda(data=slice[case_2_IV,], cls=label[case_2_IV], model=14)
        # case.2_IV_model.lmt<-LMT(labels~., data=slice_for_LMT[case_2_IV,])
        # case.2_IV_model.xgboost<-xgboost(data=as.matrix(slice[case_2_IV,]), max_depth=6, label=replacement(label[case_2_IV]), objective="multi:softmax", num_class=2, nrounds=2, eta=0.3, eval_metric="merror")
        # case.2_IV_model.lasso<-glmnet(x=as.matrix(slice[case_2_IV,]), y=label[case_2_IV], family="binomial")
          case.2_IV_model.sdwd<-sdwd(x=slice[case_2_IV,], y=label[case_2_IV], lambda2=1)
          models_repos_IV<-list(# case.2_IV_model.svm,
                                case.2_IV_model.rf,
                              # case.2_IV_model.adaboost,
                              # case.2_IV_model.hdda,
                              # case.2_IV_model.lmt,
                              # case.2_IV_model.xgboost,
                              # case.2_IV_model.lasso,
                                case.2_IV_model.sdwd)
          models_repos[[4]]<-models_repos_IV
        }# for case_2_IV
        names(models_repos)<-c("case_2_I", "case_2_II", "case_2_III", "case_2_IV")
      }
    }
    # start iterating
    for(j in 1:80){
      # jに対応する２種類の学習データを生成する
      case_num<-section(j)
      case_1<-case_num[[1]]
      # case_2<-case_num[[2]]
      # case 1
      if(1){
        # case 1. modeling
        if(1){
        # case.1_model.svm<-ksvm(x=as.matrix(slice[case_1,]), y=label[case_1], scaled=FALSE)
          case.1_model.rf<-randomForest(x=slice[case_1,], y=label[case_1])
        # case.1_model.adaboost<-boosting(labels~., data=slice_for_LMT[case_1,])
        # case.1_model.hdda<-hdda(data=slice[case_1,], cls=label[case_1], model=14)
        # case.1_model.lmt<-LMT(labels~., data=slice_for_LMT[case_1,])
        # case.1_model.xgboost<-xgboost(data=as.matrix(slice[case_1,]), max_depth=6, label=replacement(label[case_1]), objective="multi:softmax", num_class=2, nrounds=2, eta=0.3, eval_metric="merror")
        # case.1_model.lasso<-glmnet(x=slice[case_1,], y=label[case_1], family="binomial")
          case.1_model.sdwd<-sdwd(x=slice[case_1,], y=label[case_1], lambda2=1)
        }
        # case 1. prediction
        if(1){
        # res_case_1[j,1]<-as.character(predict(case.1_model.svm, twice(x=slice[j,]))[1])
          res_case_1[j,1]<-as.character(predict(case.1_model.rf, slice[j,]))
        # res_case_1[j,3]<-predict.boosting(case.1_model.adaboost, newdata=slice_for_LMT[j,])$class
        # res_case_1[j,3]<-unique_authors[predict(case.1_model.hdda, slice[j,])$class]
        # res_case_1[j,4]<-as.character(predict(case.1_model.lmt, slice[j,]))
        # res_case_1[j,4]<-as.character(predict(case.1_model.lmt, as.data.frame(twice(slice[j,]))))[1]
        # res_case_1[j,5]<-unique_authors[predict(case.1_model.xgboost, newdata=as.matrix(twice(x=slice[j,])))[1]+1]
        # res_case_1[j,6]<-predict(case.1_model.lasso, as.matrix(twice(slice[j,])), s=min(case.1_model.lasso$lambda), type="class")[1]
          res_case_1[j,2]<-ifelse((predict(case.1_model.sdwd, newx=twice(slice[j,]), s=0.005)[1])==-1, unique_authors[1], unique_authors[2])
        }
      }
      # case 2
      if(1){
        cuneiform<-(i-1)%/%20+1
      # res_case_2[j,1]<-as.character(predict(models_repos[[cuneiform]][[1]], twice(x=slice[j,]))[1])
        res_case_2[j,1]<-as.character(predict(models_repos[[cuneiform]][[1]], slice[j,]))
      # res_case_2[j,3]<-predict.boosting(models_repos[[cuneiform]][[3]], newdata=slice_for_LMT[j,])$class
      # res_case_2[j,3]<-unique_authors[predict(models_repos[[cuneiform]][[3]], slice[j,])$class]
      # res_case_2[j,4]<-as.character(predict(models_repos[[cuneiform]][[4]], slice[j,]))
      # res_case_2[j,4]<-as.character(predict(models_repos[[cuneiform]][[4]], as.data.frame(twice(slice[j,]))))[1]
      # res_case_2[j,5]<-unique_authors[predict(models_repos[[cuneiform]][[5]], newdata=as.matrix(twice(x=slice[j,])))[1]+1]
      # res_case_2[j,6]<-predict(models_repos[[cuneiform]][[6]], as.matrix(twice(slice[j,])), s=min(models_repos[[cuneiform]][[7]]$lambda), type="class")[1]
        res_case_2[j,2]<-ifelse((predict(models_repos[[cuneiform]][[2]], newx=twice(slice[j,]), s=0.005)[1])==-1, unique_authors[1], unique_authors[2])
      }
      cat(paste0("Inner: " ,j*1.25, "%\n"))
    }
    rm(models_repos)
    # rebuild
    if(1){
      Genre_A<-rbind(res_case_1[1:40, ], res_case_2[1:40, ])#予測先のジャンルで統一
      Genre_B<-rbind(res_case_1[41:80,], res_case_2[41:80,])
    }
    # siloing
    if(1){
      last_accu_A[i,]<-operator_new(x=Genre_A, lab=label)
      # last_accu_A[i,5]<-table(Genre_A[, 5], label)%>%rowMaxs%>%sum/80
      last_accu_B[i,]<-operator_new(x=Genre_B, lab=label)
      # last_accu_B[i,5]<-table(Genre_B[, 5], label)%>%rowMaxs%>%sum/80
    }
    cat(paste0("Current_i: ", i, " Outer: ", round(i/enumeration_num, 4)*100, "%\n"));beep(1)
  }
  # end
  if(1){
    # for genre A
    if(1){
      Genre_A_average_accuracies<-colMeans(last_accu_A)
      Genre_A_sd_accuracies<-apply(last_accu_A, 2, sd)
      A_Accu_sd<-rbind(Genre_A_average_accuracies, Genre_A_sd_accuracies)
      dimnames(A_Accu_sd)<-list(Statistics=c("Accu", "sd"),
                              # Classifiers=c("svm", "rf", "hdda", "lmt", "xgboost", "lasso", "sdwd"))
                                Classifiers=c("rf", "sdwd"))
    }
    # for genre B
    if(1){
      Genre_B_average_accuracies<-colMeans(last_accu_B)
      Genre_B_sd_accuracies<-apply(last_accu_B, 2, sd)
      B_Accu_sd<-rbind(Genre_B_average_accuracies, Genre_B_sd_accuracies)
      dimnames(B_Accu_sd)<-list(Statistics=c("Accu", "sd"),
                              # Classifiers=c("svm", "rf", "hdda", "lmt", "xgboost", "lasso", "sdwd"))
                                Classifiers=c("rf", "sdwd"))
    }
    # for 2 genres
    if(1){
      Total_Accu_sd<-rbind((Genre_A_average_accuracies + Genre_B_average_accuracies)/2, (Genre_A_sd_accuracies + Genre_B_sd_accuracies)/2)
      dimnames(Total_Accu_sd)<-list(Statistics=c("Accu", "sd"),
                                  # Classifiers=c("svm", "rf", "hdda", "lmt", "xgboost", "lasso", "sdwd"))
                                    Classifiers=c("rf", "sdwd"))
    }
  }
  list(last_accu_A=last_accu_A,
       last_accu_B=last_accu_B,
       A_Accu_sd=A_Accu_sd,# for genre A
       B_Accu_sd=B_Accu_sd,# for genre B
       Total_Accu_sd=rbind((Genre_A_average_accuracies+Genre_B_average_accuracies)/2,
                           Total_sd<-(Genre_A_sd_accuracies+Genre_B_sd_accuracies)/2))
}

test_res<-x|>Z_extreme_2022_binary()

Z_2022_Pairs<-function(x, pair=2, amount=40, seed=0){#引入动态阈值设定（dynamic threshold setting）,每次仿真中threshold根据pair的值改变，遵循公式：threshold=m*20
  if(1){
    library(randomForest)
    library(beepr)
    library(matrixStats)
    library(stringr)
  }
  #self-made functions
  if(1){
    #総度数が閾値を下回る変数項目を一つの変数項目に合併する関数
    shorten<-function(x, threshold=0){
      if(threshold==0){
        return(x)
      }else{
        n<-which(colSums(x)>=threshold)
        # n<-1:13
        if((ncol(x)-length(n))<=1){
          return(x)
        }else{
          return(cbind(x[,n], Others=rowSums(x[,-n])))
        }
      }
    }
    #与えられた集計度数データを相対度数データに変換する。ただし各行の和が1
    proportionate<-function(x){
      return(x/rowSums(x))
    }
    #要素が文字列であるベクトル同士を比較し、正解率とF1値を要素とするベクトルをそれぞれ出力する
    operator_accu<-function(x, lab){
      #generate 2 vectors for storaging
      nr<-length(x)
      accu<-sum(x==lab)/nr
      return(accu)
    }
    #同じベクトルを指定した回数横に重ねる
    twice<-function(x, times=2){
      result<-x
      for(i in 1:(times-1)){
        result<-rbind(result, x)
      }
      return(result)
    }
    #1から、等差が1である等差数列から、指定した数番目の指定した長さのベクトルを切り出す
    cut_out<-function(x, length = 40){
      #x can be a integer or a vector
      len<-length(x)
      res<-vector()
      for(i in 1:len){
        res<-c(res, ((x[i]-1)*length+1):(x[i]*length))
      }
      return(res)
    }
    #とあるマトリックスから、”0”を含む列を削除し、残りの列を出力
    exclude<-function(x){
      #x is a matrix where may be some impurities i.e. "0" in
      res<-which(x[1,]=="0")
      return(x[,-res])
    }
    #与えられた文字列を要素とするベクトルに対して、要素同士を繋げ、間に”_”を入れる
    stick<-function(x){
      #x is a string vector
      res<-paste(x, collapse="_")
      return(res)
    }
    #与えられた文字列と同じ長さの公差が1である等差数列を生成する、一個目の要素は指定できる、デフォルトは0：関数as.numericのヘマを片付ける
    replacement<-function(x){
      len<-length(x)
      factors_names<-unique(x)
      factors_num<-length(factors_names)
      res<-vector(length = len)
      for(i in 1:len){
        res[i]<-which(factors_names==x[i])-1
      }
      return(res)
    }
    #与えられたマトリックスに対して、NaNである要素を全て指定の値に置き換える、デフォルトは0
    no_NaN<-function(x,y=0){
      #x is a matrix may contains NaN
      x[is.nan(x)]<-y
      return(x)
    }
  }
  #set-up
  if(1){
    thre<-pair*5
    nc<-ncol(x)
    True_Variables<-colnames(x)
    Dummy_Variables<-paste0("V_", 1:nc)
    colnames(x)<-Dummy_Variables
    candidates<-c("岩井俊二", "貴志祐介", "吉田修一", "宮部みゆき", "森見登美彦", "石田衣良", "村上春樹", "村上龍",   "東野圭吾",   "湊かなえ",  "鈴木光司")
    enumeration_candidates<-t(combn(candidates, m=pair))
    author_labels<-rep(candidates, each=40)
    enumeration<-t(combn(c(1:11), m=pair))
    nr_enumeration<-nrow(enumeration)
    total_res_accuracy<-rep(0, nr_enumeration)
    names(total_res_accuracy)<-apply(enumeration_candidates, 1, stick)
  }
  #start to iteration
  for(i in 1:nr_enumeration){
    #set up for each pair
    if(1){
      numbers<-cut_out(enumeration[i,])
      # data<-as.matrix(proportionate(shorten(x[numbers,], threshold=thre)))
      data<-x[numbers,]|>proportionate()|>as.matrix()
      data<-data[, which(colSums(data)!=0)]
      labels<-as.factor(author_labels[numbers])
      unique_authors<-candidates[enumeration[i,]]
      result_for_each_pair<-rep("", pair*amount)
      names(result_for_each_pair)<-names(data)
    }
    for(j in 1:(pair*amount)){
      set.seed(seed=seed)
      model.rf<-randomForest(x=data[-j,], y=labels[-j], importance=FALSE) # modeling
      result_for_each_pair[j]<-as.character(predict(model.rf, data[j,])) # prediction
      cat(paste0("j:", j, "\t内循环:", round(2.5*j/pair, 2), "%\n")) # output progress.
    }
    #storing
    if(1){
      # result_for_each_pair<-no_NaN(x=result_for_each_pair)
      total_res_accuracy[i]<-operator_accu(x=result_for_each_pair, lab=labels)
      cat(paste0("i:", i, "\t外循环:", round(i/nr_enumeration*100, 2), "%\n"))
      beep(10)
    }
  }
  #output
  if(1){
    list(source=total_res_accuracy,
         average_accuracy=mean(total_res_accuracy),
         sd_accuracy=sd(total_res_accuracy))
  }
}

# 中身を確認。
if(0){
  rePre$Pre |> length() 
  rePre$Abb |> length()
  rePre$Pos |> length()
  
  c(rePre$Pre$Predicates_linked_G0 |> dim(),
    rePre$Pre$Predicates_linked_G1 |> dim(),
    rePre$Pre$Predicates_linked_G2 |> dim(),
    rePre$Pre$Predicates_linked_G3 |> dim())
  
  c(rePre$Abb$Abbreviated_Sentences_linked_G0 |> dim(),
    rePre$Abb$Abbreviated_Sentences_linked_G1 |> dim(),
    rePre$Abb$Abbreviated_Sentences_linked_G2 |> dim(),
    rePre$Abb$Abbreviated_Sentences_linked_G3 |> dim())
  
  c(rePre$Pos$Possible_Fragments_linked_G0 |> dim(),
    rePre$Pos$Possible_Fragments_linked_G1 |> dim(),
    rePre$Pos$Possible_Fragments_linked_G2 |> dim(),
    rePre$Pos$Possible_Fragments_linked_G3 |> dim())
  
  rePre$Pre$Predicates_linked_G0 |> colnames() |> head()
  rePre$Pre$Predicates_linked_G1 |> colnames() |> head()
  rePre$Pre$Predicates_linked_G2 |> colnames() |> head()
  rePre$Pre$Predicates_linked_G3 |> colnames() |> head()
  
  rePre$Abb$Abbreviated_Sentences_linked_G0 |> colnames() |> head()
  rePre$Abb$Abbreviated_Sentences_linked_G1 |> colnames() |> head()
  rePre$Abb$Abbreviated_Sentences_linked_G2 |> colnames() |> head()
  rePre$Abb$Abbreviated_Sentences_linked_G3 |> colnames() |> head()
  
  rePre$Pos$Possible_Fragments_linked_G0 |> colnames() |> head()
  rePre$Pos$Possible_Fragments_linked_G1 |> colnames() |> head()
  rePre$Pos$Possible_Fragments_linked_G2 |> colnames() |> head()
  rePre$Pos$Possible_Fragments_linked_G3 |> colnames() |> head()
}
# 극단적으로 불균형된 상테.
if(1){
  if(0){
    결과_1_1<-rePre[[1]][[1]] |> Z_extreme_2022_binary()
    결과_1_2<-rePre[[1]][[2]] |> Z_extreme_2022_binary()
    결과_1_3<-rePre[[1]][[3]] |> Z_extreme_2022_binary()
    결과_1_4<-rePre[[1]][[4]] |> Z_extreme_2022_binary()
    결과_1<-list(# 결과_1_1 = 결과_1_1,
                 결과_1_2 = 결과_1_2,
                 결과_1_3 = 결과_1_3,
                 결과_1_4 = 결과_1_4)
  }
  if(0){
    # 결과_2_1<-rePre[[2]][[1]] |> Z_extreme_2022_binary()
    결과_2_2<-rePre[[2]][[2]] |> Z_extreme_2022_binary()
    결과_2_3<-rePre[[2]][[3]] |> Z_extreme_2022_binary()
    결과_2_4<-rePre[[2]][[4]] |> Z_extreme_2022_binary()
    # 결과_2_4<-test_res
    결과_2<-list(# 결과_2_1 = 결과_2_1,
                 결과_2_2 = 결과_2_2,
                 결과_2_3 = 결과_2_3,
                 결과_2_4 = 결과_2_4)
  }
  if(1){
    # 결과_3_1<-rePre[[3]][[1]] |> Z_extreme_2022_binary()
    결과_3_2<-rePre[[3]][[2]] |> Z_extreme_2022_binary()
    결과_3_3<-rePre[[3]][[3]] |> Z_extreme_2022_binary()
    결과_3_4<-rePre[[3]][[4]] |> Z_extreme_2022_binary()
    결과_3<-list(# 결과_3_1 = 결과_3_1,
                 결과_3_2 = 결과_3_2,
                 결과_3_3 = 결과_3_3,
                 결과_3_4 = 결과_3_4)
  }
  # 잠시결과<-list(결과_1 = 결과_1,
  #                결과_2 = 결과_2,
  #                결과_3 = 결과_3)
}
# 보통 둘저자
if(1){
  if(1){
    # 보통결과_1_1<-rePre[[1]][[1]] |> Z_2022_Pairs(pair=2)
    보통결과_1_2<-rePre[[1]][[2]] |> Z_2022_Pairs(pair=2)
    보통결과_1_3<-rePre[[1]][[3]] |> Z_2022_Pairs(pair=2)
    보통결과_1_4<-rePre[[1]][[4]] |> Z_2022_Pairs(pair=2)
    보통결과_1<-list(# 결과_1_1 = 결과_1_1,
      보통결과_1_2 = 보통결과_1_2,
      보통결과_1_3 = 보통결과_1_3,
      보통결과_1_4 = 보통결과_1_4)
  }
  if(1){
    # 결과_2_1<-rePre[[2]][[1]] |> Z_extreme_2021_binary()
    보통결과_2_2<-rePre[[2]][[2]] |> Z_2022_Pairs(pair=2)
    보통결과_2_3<-rePre[[2]][[3]] |> Z_2022_Pairs(pair=2)
    보통결과_2_4<-rePre[[2]][[4]] |> Z_2022_Pairs(pair=2)
    보통결과_2<-list(# 결과_2_1 = 결과_2_1,
      보통결과_2_2 = 보통결과_2_2,
      보통결과_2_3 = 보통결과_2_3,
      보통결과_2_4 = 보통결과_2_4)
  }
  if(1){
    # 결과_3_1<-rePre[[3]][[1]] |> Z_extreme_2021_binary()
    보통결과_3_2<-rePre[[3]][[2]] |> Z_2022_Pairs(pair=2)
    보통결과_3_3<-rePre[[3]][[3]] |> Z_2022_Pairs(pair=2)
    보통결과_3_4<-rePre[[3]][[4]] |> Z_2022_Pairs(pair=2)
    보통결과_3<-list(# 결과_3_1 = 결과_3_1,
      보통결과_3_2 = 보통결과_3_2,
      보통결과_3_3 = 보통결과_3_3,
      보통결과_3_4 = 보통결과_3_4)
  }
}

save(보통결과_1, file = "/Users/liuyejia_2021/Downloads/보통결과_1.rdata")
save(보통결과_2, file = "/Users/liuyejia_2021/Downloads/보통결과_2.rdata")
save(보통결과_3, file = "/Users/liuyejia_2021/Downloads/보통결과_3.rdata")

# 보통 셋저자
if(1){
  if(1){
    # 보통결과_1_1셋<-rePre[[1]][[1]] |> Z_2022_Pairs(pair=2)
    보통결과_1_2셋<-rePre[[1]][[2]] |> Z_2022_Pairs(pair=3)
    보통결과_1_3셋<-rePre[[1]][[3]] |> Z_2022_Pairs(pair=3)
    보통결과_1_4셋<-rePre[[1]][[4]] |> Z_2022_Pairs(pair=3)
    보통결과_1셋<-list(# 결과_1_1 = 결과_1_1,
      보통결과_1_2셋 = 보통결과_1_2셋,
      보통결과_1_3셋 = 보통결과_1_3셋,
      보통결과_1_4셋 = 보통결과_1_4셋)
  }
  if(1){
    # 결과_2_1<-rePre[[2]][[1]] |> Z_extreme_2021_binary()
    보통결과_2_2셋<-rePre[[2]][[2]] |> Z_2022_Pairs(pair=3)
    보통결과_2_3셋<-rePre[[2]][[3]] |> Z_2022_Pairs(pair=3)
    보통결과_2_4셋<-rePre[[2]][[4]] |> Z_2022_Pairs(pair=3)
    보통결과_2셋<-list(# 결과_2_1 = 결과_2_1,
      보통결과_2_2셋 = 보통결과_2_2셋,
      보통결과_2_3셋 = 보통결과_2_3셋,
      보통결과_2_4셋 = 보통결과_2_4셋)
  }
  if(1){
    # 결과_3_1<-rePre[[3]][[1]] |> Z_extreme_2021_binary()
    보통결과_3_2셋<-rePre[[3]][[2]] |> Z_2022_Pairs(pair=3)
    보통결과_3_3셋<-rePre[[3]][[3]] |> Z_2022_Pairs(pair=3)
    보통결과_3_4셋<-rePre[[3]][[4]] |> Z_2022_Pairs(pair=3)
    보통결과_3셋<-list(# 결과_3_1 = 결과_3_1,
      보통결과_3_2셋 = 보통결과_3_2셋,
      보통결과_3_3셋 = 보통결과_3_3셋,
      보통결과_3_4셋 = 보통결과_3_4셋)
  }
}

결과_3[[1]]$Total_Accu_sd[1,]
결과_3[[2]]$Total_Accu_sd[1,]
결과_3[[3]]$Total_Accu_sd[1,]

결과_3$결과_3_4$Total_Accu_sd[,2]

# 십군 분류 때 각 특징량 데이터 세트 단체의 성능를 계속한다.
if(0){
  # M0
  Test_Result_M0<-new_right_Features_Pool[[19]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  # M1
  Test_Result_M1G1<-complete_rePre[[1]][[2]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M1G2<-complete_rePre[[1]][[3]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M1G3<-complete_rePre[[1]][[4]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  # M2
  Test_Result_M2G1<-complete_rePre[[2]][[2]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M2G2<-complete_rePre[[2]][[3]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M2G3<-complete_rePre[[2]][[4]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  # M3
  Test_Result_M3G1<-complete_rePre[[3]][[2]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M3G2<-complete_rePre[[3]][[3]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M3G3<-complete_rePre[[3]][[4]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  # M4
  Test_Result_M4G1<-complete_rePre[[4]][[2]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M4G2<-complete_rePre[[4]][[3]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M4G3<-complete_rePre[[4]][[4]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  # M5
  Test_Result_M5G1<-complete_rePre[[5]][[2]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M5G2<-complete_rePre[[5]][[3]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
  Test_Result_M5G3<-complete_rePre[[5]][[4]]|>Z_2022_Pairs_십(pair=10, thre_switch=1, thre=10)
}

# 십군 분류 때 정밀한 연결된 복합 특징량 [M0, M5G3] 성능
sample(x=9:14, 1)
# 중요한 변수를 선택함
if(1){
  변수선택함수<-function(x){
    set.seed(0)
    library(randomForest)
    shorten<-function(x, threshold=0){
      if(threshold==0){
        return(x)
      }else{
        n<-which(colSums(x)>=threshold)
        # n<-1:13
        if((ncol(x)-length(n))<=1){
          return(x)
        }else{
          return(cbind(x[,n], Others=rowSums(x[,-n])))
        }
      }
    }
    proportionate<-function(x){
      return(x/rowSums(x))
    }
    # x<-M0
    라벨<-LETTERS[1:10]|>rep(each=40)|>as.factor()
    data<-x|>shorten(threshold=10)|>proportionate()|>as.matrix()
    모델<-randomForest(x=data, y=라벨, importance=T, norm.votes=T, proximity=T)
    return(모델)
    # 모델$importance|>rownames()|>head(100)
  }
  system.time(모델_M0_rf  <-M0  |>변수선택함수())
  system.time(모델_M5G3_rf<-M5G3|>변수선택함수())
}
library(stringr)
# M0
순서_1<-모델_M0_rf$importance[,12]|>order(decreasing = T)|>head(101)
# M5G3
순서_2<-모델_M5G3_rf$importance[,12]|>order(decreasing = T)|>head(101)
모델_M0_rf$importance[순서_1, 12]|>round(4)
모델_M5G3_rf$importance[순서_2, 12]|>round(4)
# 모델_M0_rf$importance|>rownames()|>head(20)|>str_replace_all("_", "-&-")
# 모델_M5G3_rf$importance|>rownames()|>head(20)|>str_replace_all("야", "-&-")
모임<-cbind(모델_M0_rf$importance[순서_1, 12]|>round(4),
            모델_M5G3_rf$importance[순서_2, 12]|>round(4))
모임_변수명<-cbind(rownames(모델_M0_rf$importance)[순서_1],
                   rownames(모델_M5G3_rf$importance)[순서_2])
# 모임_변수명[,1]|>str_detect("\\.\\.1")|>which()
모임_변수명[,1]<-모임_변수명[,1]|>str_replace_all("\\.\\.1", "。")|>str_replace_all("_\\.$", "、")
모임_변수명[,2]<-모임_변수명[,2]|>str_replace_all("_", "=")|>str_replace_all("야", "_")

write.csv(모임, file="/Users/liuyejia_2021/Downloads/모임.csv")
write.csv(모임_변수명, file="/Users/liuyejia_2021/Downloads/모임_변수명.csv")

plot(1:50,모임[,1], )
plot(1:50,모임[,2])

차<-모임[,2]-모임[,1] # M5G3 - M0 > 0 이 조건을 만족시킨 요소 위치을 특정하다.
sum(차>0) # 조건을 만족시킨 요소 수.
우승위치<-which(차>0)
모임_변수명[우승위치,2] # 조건을 만족시킨 요소들 이름.


BNS특유<-c(0,1,0,1,1,1,1,1,0,1,1,1,1,0,1,2,1,0,1,0,1,1,0,1,1,1,0,1,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,1,1,1,1,0,1,0,1,0,0,0,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,0,1,0,1,0,0,0,1,0,1,1,1,0,1,1,0,1,0,1,0,1,1,1,1,1,1)
BNS특유<-BNS특유[-16]
특유성있음<-which(BNS특유==1)
pch_BNS특유<-BNS특유
pch_BNS특유[which(pch_BNS특유==1)]<-17
pch_BNS특유[which(pch_BNS특유==0)]<-1
plot( x=1:100, y=모임[-16,2], type="b", pch=pch_BNS특유,
      # col=BNS특유+1,
      ylab="MeanDecreaseGini", xlab="Variables of M5G3")
lines(x=1:100, y=모임[-25,1], type="l", col="gray")
legend("topright", legend=c("M0", "M5G3"), lty=c(1,2), col=c("gray", "black"), cex=0.8)
grid(lty=3)

# 10 folders for multi-discrimination.
# 자작 함수 군.
if(1){
  shorten<-function(x, threshold=0){
    if(threshold==0){
      return(x)
    }else{
      n<-which(colSums(x)>=threshold)
      if(ncol(x)-length(n)>=2){
        return(cbind(x[, n], Others=rowSums(x[, -n])))
      }else{
        return(x)
      }
    }
  }
  proportion<-function(x){
    return(x/rowSums(x))
  }
  fun_ramification<-function(x, L=40, l=20){
    len_x<-length(x)
    a<-(x-1)*L+1
    b<-a+l-1
    res<-NULL
    for(i in 1:len_x){
      res<-c(res, a[i]:b[i])
    }
    return(res)
  }
  fun_split<-function(x, k){
    # xは正の整数を要素として持つベクトルであり、kはkフォルダの数を指定する。
    # fun_split関数はベクトルxをk等分するものであり、
    # もし余りがあって、その長さがn(n<k)である場合、それをランダムにn個のフォルダに付け加える。
    library(stringr)
    res<-list()
    len_x<-length(x)
    num_per<-floor(len_x/k)
    for(i in 1:k){
      a<-x%>%sample(num_per)%>%sort()->res[[i]]
      x<-setdiff(x, a)
    }
    len_x<-length(x)
    if(len_x!=0){
      b<-sample(1:k, len_x)
      for(i in 1:len_x){
        res[[b[i]]]<-c(res[[b[i]]], x[i])%>%sort()
      }
    }
    names(res)<-paste("Folder", 1:k, sep="_")
    return(res)
  }
  fun_k_folders<-function(x, k=10, l=40, pos="both"){
    x<-sort(x)
    library(stringr)
    num_class<-x %>% length()
    a<-list()
    for(i in 1:num_class){
      a[[i]]<-((x[i]-1)*l+1):(x[i]*l)
    }
    a<-a %>% unlist() %>% matrix(l, num_class)
    a_up<-a[1:floor(l/2), ]
    a_down<-a[-(1:floor(l/2)), ]
    a_vec<-as.vector(a)
    a_up_vec<-as.vector(a_up)
    a_down_vec<-as.vector(a_down)
    # stratified structure: classes -> genres -> folders.
    # 階層構造：クラス -> ジャンル -> フォルダ． 
    RES<-list()
    for(i in 1:num_class){
      # 对每列的上半段与下半段分别进行切分（split）
      up_k<-a_up[, i] %>% fun_split(k)
      down_k<-a_down[, i] %>% fun_split(k)
      RES[[i]]<-list(up_k=up_k, down_k=down_k)
    }
    b_up<-matrix(0,
                 k,
                 num_class)->b_down
    for(i in 1:num_class){
      b_up[, i]<-sample(1:k, k)
      b_down[, i]<-sample(1:k, k)
    }
    FINAL_RES<-list()
    for(i in 1:k){
      b_up_test<-list()
      b_down_test<-list()
      for(j in 1:num_class){
        b_up_test[[j]]<-RES[[j]][[1]][[b_up[i, j]]]
        b_down_test[[j]]<-RES[[j]][[2]][[b_down[i, j]]]
      }
      b_up_test<-b_up_test %>% unlist()
      b_down_test<-b_down_test %>% unlist()
      b_up_train<-setdiff(a_up_vec, b_up_test)
      b_down_train<-setdiff(a_down_vec, b_down_test)
      UP<-list(up_test=b_up_test,
               up_train=b_up_train)
      DOWN<-list(down_test=b_down_test,
                 down_train=b_down_train)
      COMBINED<-list(test=sort(c(b_up_test, b_down_test)),
                     train=sort(c(b_up_train, b_down_train)))
      if(pos=="both"){
        FINAL_RES[[i]]<-COMBINED
      }else{
        # if(pos=="up"){
        #   FINAL_RES[[i]]<-UP
        # }else{
        #   FINAL_RES[[i]]<-DOWN
        # }
        FINAL_RES[[i]]<-list(UP=UP, DOWN=DOWN)
      }
    }
    names(FINAL_RES)<-paste("folder", 1:k, sep="_")
    return(FINAL_RES)
  }
  operator_accu<-function(x, lab){
    #generate 2 vectors for storaging
    nr<-length(x)
    accu<-sum(x==lab)/nr
    return(accu)
  }
}
fun_십십<-function(x, seed=0, thre=10){ # 십군판별 & 십 폴더 교차검증법 함수.
  if(0){
    x<-M5G3
    seed<-2
    thre<-10
    i<-7
  }
  set.seed(seed=seed)
  라벨<-LETTERS[1:10]|>rep(each=40)|>as.factor()
  십폴더<-fun_k_folders(x=1:10, k=10)
  정답률<-NULL
  for(i in 1:10){
    # 학습용 데이터 작성.
    if(1){
      학습용<-x[십폴더[[i]]$train, ]
      학습용_라벨<-라벨[십폴더[[i]]$train]
      열_합<-학습용|>colSums()
      남음번호<-열_합>=thre
      학습용<-cbind(학습용[, 남음번호], Others=rowSums(학습용[, -남음번호]))
      학습용<-학습용|>proportion()  
    }
    # 테스트용 데이터 작성.
    if(1){
      테스트용<-x[십폴더[[i]]$test,]
      테스트용<-cbind(테스트용[, 남음번호], Others=rowSums(테스트용[, -남음번호]))
      테스트용<-테스트용|>proportion()
      테스트용_라벨<-라벨[십폴더[[i]]$test]
    }
    지금모델<-randomForest(x=학습용, y=학습용_라벨)
    이번예측결과<-as.character(predict(지금모델, 테스트용))
    이번예측결과|>length()
    이번정답률<-sum(이번예측결과==as.character(테스트용_라벨))/length(이번예측결과)
    정답률<-c(정답률, 이번정답률)
    cat(i, "th folder Done!\n")
  }
  まとめ<-list(소스=정답률,
            평균정답률=정답률|>mean(),
            표준편차치=정답률|>sd())
}
십십_M0_s512  <-M0  |>fun_십십(seed = 512)
십십_M5G3_s512<-M5G3|>fun_십십(seed = 512)


십십_M5G3
십십_M0

십십_M5G3_s100
십십_M0_s100

# seed = 10^n, n∈[0, 1, 2, 3]
#seed=0, threshold=10
십십_M0 # 0.9425(0.0541)
십십_M5G3 # 0.9425(0.0472)
# seed=10
십십_M0_s10 # 0.94(0.0428)
십십_M5G3_s10 # 0.93(0.0453)
# seed=100
십십_M0_s100 # 0.9375(0.0317)
십십_M5G3_s100 # 0.93(0.0599)
# seed = 1000
십십_M0_s1000 # 0.93(0.0483)
십십_M5G3_s1000 # 0.93(0.0438)

# seed = 2^n, n∈[0, 1, 2, 3]
if(1){
  # 1. seed=0
  십십_M0 # 0.9425(0.0541)
  십십_M5G3 # 0.9425(0.0472)
  # 2. seed=2
  십십_M0_s2 # 0.93(0.0511)
  십십_M5G3_s2 # 0.9275(0.0432)
  # 3. seed=4
  십십_M0_s4 # 0.9225(0.0381)
  십십_M5G3_s4 # 0.9325(0.0426)
  # 4. seed=8
  십십_M0_s8 # 0.9325(0.0409)
  십십_M5G3_s8 # 0.9378(0.0339)
  # 5. seed=16
  십십_M0_s16 # 0.925(0.0289)
  십십_M5G3_s16 # 0.93(0.0350)
  # 6. seed=32
  십십_M0_s32 # 0.935(0.0428)
  십십_M5G3_s32 # 0.9375(0.0358)
  # 7. seed=64
  십십_M0_s64 # 0.925(0.0408)
  십십_M5G3_s64 # 0.9375(0.0295)
  # 8. seed=128
  십십_M0_s128 # 0.95(0.0373)
  십십_M5G3_s128 # 0.9375(0.0637)
  # 9. seed=256
  십십_M0_s256 # 0.9275(0.0184)
  십십_M5G3_s256 # 0.94(0.0293)
  # 10. seed=512
  십십_M0_s512 # 0.9325(0.0514)
  십십_M5G3_s512 # 0.9425(0.0265)
}

# 십 시드 패키지화 함수.
패키지화<-function(x){
  멱<-0:9; 시드들<-2^멱
  매회평균정답률<-rep(0, 10)
  매회표준편차치<-rep(0, 10)
  for(i in 1:10){
    그때결과<-x|>fun_십십(seed = 시드들[i])
    매회평균정답률[i]<-그때결과$평균정답률
    매회표준편차치[i]<-그때결과$표준편차치
  }
  list(매회평균정답률=매회평균정답률,
       매회표준편차치=매회표준편차치)
}


if(1){
  # M1
  M1G1_10군10폴더<-complete_rePre[[1]][[2]]|>패키지화()
  M1G2_10군10폴더<-complete_rePre[[1]][[3]]|>패키지화()
  M1G3_10군10폴더<-complete_rePre[[1]][[4]]|>패키지화()
  # M2
  M2G1_10군10폴더<-complete_rePre[[2]][[2]]|>패키지화()
  M2G2_10군10폴더<-complete_rePre[[2]][[3]]|>패키지화()
  M2G3_10군10폴더<-complete_rePre[[2]][[4]]|>패키지화()
  # M3
  M3G1_10군10폴더<-complete_rePre[[3]][[2]]|>패키지화()
  M3G2_10군10폴더<-complete_rePre[[3]][[3]]|>패키지화()
  M3G3_10군10폴더<-complete_rePre[[3]][[4]]|>패키지화()
  # M4
  M4G1_10군10폴더<-complete_rePre[[4]][[2]]|>패키지화()
  M4G2_10군10폴더<-complete_rePre[[4]][[3]]|>패키지화()
  M4G3_10군10폴더<-complete_rePre[[4]][[4]]|>패키지화()
  # M5
  M5G1_10군10폴더<-complete_rePre[[5]][[2]]|>패키지화()
  M5G2_10군10폴더<-complete_rePre[[5]][[3]]|>패키지화()
  M5G3_10군10폴더<-complete_rePre[[5]][[4]]|>패키지화()
}

f_추출<-function(x){
  return(c(x[[1]]|>mean(), x[[2]]|>mean()))
}

모음<-rbind(M1G1_10군10폴더|>f_추출(),
            M1G2_10군10폴더|>f_추출(),
            M1G3_10군10폴더|>f_추출(),
            M2G1_10군10폴더|>f_추출(),
            M2G2_10군10폴더|>f_추출(),
            M2G3_10군10폴더|>f_추출(),
            M3G1_10군10폴더|>f_추출(),
            M3G2_10군10폴더|>f_추출(),
            M3G3_10군10폴더|>f_추출(),
            M4G1_10군10폴더|>f_추출(),
            M4G2_10군10폴더|>f_추출(),
            M4G3_10군10폴더|>f_추출(),
            M5G1_10군10폴더|>f_추출(),
            M5G2_10군10폴더|>f_추출(),
            c(0.9365, 0.0387))
          # M5G3_10군10폴더|>f_추출())
rownames(모음)<-str_c("M", rep(1:5, each=3), "G", rep(1:3, 5))
colnames(모음)<-c("accu", "sd")
# write.csv(모음, file="/Users/liuyejia_2021/Downloads/모음.csv")
뉴모음<-모음|>round2(4)
뉴모음_concatenated<-str_c(뉴모음[,1], "±", 뉴모음[,2])
names(뉴모음_concatenated)<-rownames(모음)
write.csv(뉴모음_concatenated, file="/Users/liuyejia_2021/Downloads/뉴모음_concatenated.csv")
