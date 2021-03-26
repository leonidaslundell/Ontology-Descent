[ -d data ] || mkdir data/
[ -d tmpdata ] || mkdir tmpdata/
[ -d Sub ] || mkdir Sub
[ -d Ave ] || mkdir Ave
[ -d Fuz ] || mkdir Fuz
[ -d Fin ] || mkdir Fin
#background in color
# [[ ! -f $bg_img ]] && { echo "ffmpeg1" && ffmpeg -hide_banner -loglevel warning -i "$video_in" -vf setsar=1:1,fps=1 -ss 00:00:01 -to 00:00:02 "$bg_img"; } || echo "bg exists!"
# image stplit in gray
if ! ls frames | grep -qm 1 png; then     echo "ffmpeg2" && ffmpeg -hide_banner -loglevel warning -i "$video_in" -vf setsar=1:1,fps=10,format=gray "frames/img%12d.png"; else     echo "frames exists"; fi
cat ~/.bashrc 
submitJob 60:00 40 ./difrofuz.NewTake.V1.sh 
ls
qstat
ls
head frames
ls frames
ll frames
qst
qstat
ls
cat difrofuz.NewTake.V1.sh.e30131341 
ll
nano difrofuz.NewTake.V1.sh
./difrofuz.NewTake.V1.sh 
ls
ll
rm -r Fuz Ave/ frames Sub/
ls
rm Fin/
ls tmpdata/
rm tmpdata/
rm -r tmpdata/
ls
rm difrofuz.NewTake.V1.sh.*
./difrofuz.NewTake.V1.sh 
ls
ls frames
rm -r difrofuz.NewTake.V1.sh 
module load tools
module load imagemagick/7.0.8-42
module load ffmpeg/4.1.3
module load intel/compiler/64 gcc R/3.5.0 
module load parallel/20200922
# cd /home/people/leolun/projects/videomics
# base_dir='/c/Users/qwm753/Documents/SharedData/VideomicsV2'
base_dir='/home/people/leolun/projects/videomics'
# video_in="data/vid.mp4"
video_in="data/1.mp4"
bg_img="data/background.png"
cd "$base_dir" || { echo "FAILED TO CDbg_img="data/background.png"! Exiting" && exit 1; }
# Create dirs:
[ -d frames ] || mkdir frames/
[ -d data ] || mkdir data/
[ -d tmpdata ] || mkdir tmpdata/
[ -d Sub ] || mkdir Sub
[ -d Ave ] || mkdir Ave
[ -d Fuz ] || mkdir Fuz
[ -d Fin ] || mkdir Fin
#background in color
# [[ ! -f $bg_img ]] && { echo "ffmpeg1" && ffmpeg -hide_banner -loglevel warning -i "$video_in" -vf setsar=1:1,fps=1 -ss 00:00:01 -to 00:00:02 "$bg_img"; } || echo "bg exists!"
# image stplit in gray
if ! ls frames | grep -qm 1 png; then     echo "ffmpeg2" && ffmpeg -hide_banner -loglevel warning -i "$video_in" -vf setsar=1:1,fps=10,format=gray "frames/img%12d.png"; else     echo "frames exists"; fi
function framesGen {     files=$1;      IFS=' '; 
    read -a f <<< "$files";      convert "frames/${f[2]}" "frames/${f[1]}" -compose minus -composite "Sub/${f[1]}";  }
export -f framesGen
echo "Starting composite"
Rscript /home/people/leolun/projects/videomics/rollingConcat.R Sub $(ls frames/) > fil
sed 's/"//g' fil | tee fil1
parallel -a fil1 framesGen
module load tools
module load imagemagick/7.0.8-42
module load ffmpeg/4.1.3
module load intel/compiler/64 gcc R/3.5.0 
module load parallel/20200922
# cd /home/people/leolun/projects/videomics
# base_dir='/c/Users/qwm753/Documents/SharedData/VideomicsV2'
base_dir='/home/people/leolun/projects/videomics'
# video_in="data/vid.mp4"
video_in="data/1.mp4"
bg_img="data/background.png"
cd "$base_dir" || { echo "FAILED TO CDbg_img="data/background.png"! Exiting" && exit 1; }
# Create dirs:
[ -d frames ] || mkdir frames/
[ -d data ] || mkdir data/
[ -d tmpdata ] || mkdir tmpdata/
[ -d Sub ] || mkdir Sub
[ -d Ave ] || mkdir Ave
[ -d Fuz ] || mkdir Fuz
[ -d Fin ] || mkdir Fin
#background in color
# [[ ! -f $bg_img ]] && { echo "ffmpeg1" && ffmpeg -hide_banner -loglevel warning -i "$video_in" -vf setsar=1:1,fps=1 -ss 00:00:01 -to 00:00:02 "$bg_img"; } || echo "bg exists!"
# image stplit in gray
if ! ls frames | grep -qm 1 png; then     echo "ffmpeg2" && ffmpeg -hide_banner -loglevel warning -i "$video_in" -vf setsar=1:1,fps=10,format=gray "frames/img%12d.png"; else     echo "frames exists"; fi
function framesGen {     files=$1;      IFS=' '; 
    read -a f <<< "$files";      convert "frames/${f[2]}" "frames/${f[1]}" -compose minus -composite "Sub/${f[1]}";  }
export -f framesGen
echo "Starting composite"
Rscript /home/people/leolun/projects/videomics/rollingConcat.R Sub $(ls frames/) > fil
sed 's/"//g' fil | tee fil1
parallel -a fil1 framesGen
ls
module load tools
module load imagemagick/7.0.8-42
module load ffmpeg/4.1.3
module load intel/compiler/64 gcc R/3.5.0 
module load parallel/20200922
# cd /home/people/leolun/projects/videomics
# base_dir='/c/Users/qwm753/Documents/SharedData/VideomicsV2'
base_dir='/home/people/leolun/projects/videomics'
# video_in="data/vid.mp4"
video_in="data/1.mp4"
bg_img="data/background.png"
cd "$base_dir" || { echo "FAILED TO CDbg_img="data/background.png"! Exiting" && exit 1; }
# Create dirs:
[ -d frames ] || mkdir frames/
[ -d data ] || mkdir data/
[ -d tmpdata ] || mkdir tmpdata/
[ -d Sub ] || mkdir Sub
[ -d Ave ] || mkdir Ave
[ -d Fuz ] || mkdir Fuz
[ -d Fin ] || mkdir Fin
#background in color
# [[ ! -f $bg_img ]] && { echo "ffmpeg1" && ffmpeg -hide_banner -loglevel warning -i "$video_in" -vf setsar=1:1,fps=1 -ss 00:00:01 -to 00:00:02 "$bg_img"; } || echo "bg exists!"
# image stplit in gray
if ! ls frames | grep -qm 1 png; then     echo "ffmpeg2" && ffmpeg -hide_banner -loglevel warning -i "$video_in" -vf setsar=1:1,fps=10,format=gray "frames/img%12d.png"; else     echo "frames exists"; fi
function framesGen {     files=$1;      IFS=' '; 
    read -a f <<< "$files";      convert "frames/${f[2]}" "frames/${f[1]}" -compose minus -composite "Sub/${f[1]}";  }
export -f framesGen
echo "Starting composite"
Rscript /home/people/leolun/projects/videomics/rollingConcat.R Sub $(ls frames/) > fil
sed 's/"//g' fil | tee fil1
parallel -a fil1 framesGen
nano difrofuz.V1.sh
chmod +x difrofuz.V1.sh 
nano ~/.bashrc 
submitJob 60:00 40 difrofuz.V1.sh 
ls
rm -r Sub/ frames/
qstat
ls
rm -r Ave/ Fin/ Fuz/ tmpdata/
ls
lss
ls
rm -r Fin/
ls
qstat
ls
qstat
ll frames | wc
ll -h
qstat
ll -h
ll
qstat
ll
rm -r framesAll/
qstat
ls projects/videomics/
ll projects/videomics/
qstat
ls
cd projects/videomics/
ls
ll
ls Ave
ls frames/
ls
qstat
ll
ls Ave
ls Sub/ | tail
showq
showq |less
ls
nano difrofuz.V1.sh 
qstat
mjobctl -F 30131491.moab 
ls
mv frames/ framesAll/
mv Sub/ SubOld/
ls
mv Ave/ AveOl/
ls
ls Fuz/
submitJob 60:00 40 difrofuz.V1.sh
ls
rm difrofuz.V1.sh.*
ls
qstat
ls
cd projects/videomics/
ls
rm RBkYuy 
r, f6cR11 
rm f6cR11 
ls
ll -h
rm -rf frames
ll h
ll -h
rm -rf frames/*
ll -h
nano test.sh
chmod +x test.sh 
ls
./test.sh 
ls
ll
ls Fuz/
./test.sh 
ls
exit
cd projects/videomics/
ls
ll
ll | less
ls Ave | tail
ls Sub | tail
ls
rm 3WyYVO 
rm 064CbZ 
rm 5*
rm 6ELERn 
rm 7feFxd 
rm 95AvSO 
rm b1ZDnx 
rm BwM90i 
rm ckjHKA 
rm dy9eds 
rm fmnEcj 
rm GEUkLV 
rm gnM5U3 
rm hY1fDl 
rm iradQ6 
rm j
rm j*
rm KkWKc3 
rm LbPXUm 
rm lc4vVO 
rm mhRMl3 
rm QGz60X 
rm r9Xfx9 
rm rWuQzH 
ls
rm sjdaX6 
rm SsQCSk 
rm sv5ukT 
rm ttXwHR 
rm urDAj0 
rm VX1j5c 
rm y*
rm Z*
mkdir 
mkdir submitScriptHere
iqsub
iqsub 
cd projects/videomics/
ls
./test.sh 
cd projects/
cd videomics/
ls
ll -h
ls Sub |wc
ls Ave |wc
ls Fuz/ |wc
ls Ave/ |wc
ls Fuz/ |wc
ls
top
ls
cd projects/
cd videomics/
ls
module load tools
module load ffmpeg/4.1.3 
ffmpeg -r 25 -f image2 -s 640x480 -i Fuz/img%12d.png -pix_fmt yuv420p -vf "tblend=average,framestep=2,setpts=0.50*PTS"  test2.mp4
ls
ll -h
pwd
rsync -a --delete ./frames/
ls
top
ls
rsync -a --delete ./frames/ ./
ls
ll frames |wc
mkdir empty dir
rm dir
ls
ll
rm -r dir
rsync -a --delete empty/ frames/
ls
ls empty/
rsync -a --delete Ave/ frames/
ls
rsync -a --delete Ave/ frames/ > deleteFiles.sh
echo 'rsync -a --delete Ave/ frames/ > deleteFiles.sh
echo 'rsync -a --delete Ave/ frames/' > deleteFiles.sh
ls
cat deleteFiles.sh 
exit
iqsub 
module load tools ngs
module load R
module load R/4.0.0
module load gcc
module load R/4.0.0
module load intel/perflibns
module load intel/perflibs
module load R/4.0.0
r
R
ls
cd projects/WP1_transcriptomics/
ls
cat inputdata.
cat inputdata.R
head inputdata.R
R
ls
pwd
exit
cd projects/
ls
cd WP1_transcriptomics/
ls
cd data
ls
cd ..
ls
iqsub
module load tools ngs
module load intel/perflibs gcc
module load R
module load R/4.0.0
R
iqsu
iqsub
cd projects/WP1_transcriptomics/
ls
module load tools ngs intel/perflibs gcc R/4.0.0
R
owd
pwd
ls
cd projects/
ls
cd g
ls
cd ~/
pwd
id
cd projects/
ls
cd ~/
id
q
exit
cd home/projects/ku_fa
cd /home/projects/ku_fa/
ls
id
ls
cd people/
ls
cd leolun/
ls
module load tools ngs
module load samtools/1.9
samtools --help
module load gcc intel/perflibs R/4.0.0
R --help
R --help | help
R --help | less
module load anaconda
module load anaconda/4.0.0
module purge
module load tools ngs
modele load python
module load python
module load anaconda/4.0.0 
conda --help
conda list
iqsub
exit
echo $PATH
ls
cd projects/
ls
cd 180420_RESTRICT_Hawley/
ls
ls analysis/
cat sample80whichcrashed.sh
cat sample80whichcrashed.sh.e22463155 
ls
cat comments.on.files.txt 
cat comments.on.star.bug.txt 
ls
cat cluster.call.3.sh
ls
ll -h analysis
ll -h analysis/star/
ll -h analysis/
ls
ll RESTRICT-67385318/
s
ls
cd RESTRICT-67385318/
ls
mkdir script
cp ../cluster.call.3.sh ./
cat cluster.call.3.sh 
cp ~/scripts/pipeline.no.trim.V3/combine_lanes.sh ./
cp ~/scripts/pipeline.no.trim.V3/star.sh ./
cp ~/scripts/pipeline.no.trim.V3/feature.sh ./
cp ~/scripts/pipeline.no.trim.V3/fastqc_script.sh ./
ls
mv *.sh ./script/
ls
ls script/
cd ..
ls
cp Sample\ sheet.xlsx RESTRICT-67385318/
ls
RESTRICT-67385318/
ls
cd RESTRICT-67385318/
ls
ls script/
cat script/cluster.call.3.sh 
nano script/cluster.call.3.sh 
ls
cd ..
ls
cat cluster.call.3.sh.e20319555 
cat cluster.call.3.sh.o20319555 
ls
cat cluster.call.3.sh
cat RESTRICT-67385318/script/cluster.call.3.sh 
ls
mv ./RESTRICT-67385318 ./TimeRestrictedFeedingNatCom2020
ls
cd ../
ls
cd 180420_RESTRICT_Hawley/
ls
mv TimeRestrictedFeedingNatCom2020/ ./
mv TimeRestrictedFeedingNatCom2020/ ../
ls ..
cd ..
pwd
ls
cd 180420_RESTRICT_Hawley/
ls
ll
mkdir logs
cp cluster.call.3.sh.e20319555 
cp cluster.call.3.sh.e20319555 ./logs/
ls
ll logs/
ls
rm logs
rm -r logs
cd ../lroin/
ls
module load tools ngs
module load tools intel/perflibs/2020_update1
module load tools intel/perflibs/2020_update2
module load gcc/10.2.0 
module load R
module load R/4.0.
module load R/4.0.0
R
module load glibc/2.31
R
top
module purge 
module purge
R
q()
q
top
exit
cd projects/
ls
pwd
ls
mkdir ontologyDescent
cd ontologyDescent/
pwd
ls
iqsub
quit
q
exit
module load tools
module load R
module load intel/perflibs/64/2020_update2 gcc/9.3.0 
module load R/4.0.0
R
$ strings /usr/lib64/libstdc++.so.6 | grep GLIBCXX
strings /usr/lib64/libstdc++.so.6 | grep GLIBCXX
module unload gcc/9.3.0 
module unload R/4.0.0 
module unload gcc/9.3.0 
module load gcc/10.2.0 
strings /usr/lib64/libstdc++.so.6 | grep GLIBCXX
ls usr/lib64/
ls /usr/lib64
module load glibc/2.26
module load R
R
module load R/4.0.0
R
ls
module purge
ls
exit
iqsub
cd /home/projects/ku_10011/data/projects/local/lewin_resequencing
ls
ls ..
cd ~/projects/201001_PhotoPeriod/
l
ls
cat test
rm test
ls
cd scripts/
ls
cd ..
ls fastq/
ls
ls analysis/
ls analysis/featurecounts/
ls
ls /home/projects/ku_10011/data/projects/local/lewin_resequencing
ls
cd analysis/
ls
ls final/n
ls final/
ls run1
ls final/run1
ls ../fastq/
ls ../fastq/run1
ls 
cd ..
ls
ls scripts/
cat scripts/cluster.call.3.mouse.sh 
ls analysis/final/
ls
cd ..
ls
cd 201001_PhotoPeriod/
ls
cp /home/projects/ku_10011/data/projects/local/lewin_resequencing ./fastqReRun
ls
echo "some samples were suspicious, so we reran only those" > commnetOnFastqReRun
ls
mkdir fastqReRun
cd fastqReRun/
cp /home/projects/ku_10011/data/projects/local/lewin_resequencing/* :/
cp /home/projects/ku_10011/data/projects/local/lewin_resequencing/* ./
 ls
cd ..
ls
cd scripts/
ls
cp cluster.call.3.mouse.sh ./cluster.call.3.mouse.ReRun.sh 
nano cluster.call.3.mouse.ReRun.sh 
ls ../logs/
cd ../
cat ~/.bashrc 
submitJob 24:00:00 40 ./scripts/
ls
ls scripts/cluster.call.3.mouse.ReRun.sh 
lhead scripts/cluster.call.3.mouse.ReRun.sh 
head scripts/cluster.call.3.mouse.ReRun.sh 
cat scripts/cluster.call.3.mouse.ReRun.sh 
submitJob 24:00:00 40 ./scripts/cluster.call.3.mouse.ReRun.sh 
showq
showq -h
showq -w leolun
qsta
qstat
ls
ls analysisReRun/
ls analysisReRun/combine/
ls analysis/combine/
ls analysisReRun/star/
qstat
exit
qstat
cd projects/201001_PhotoPeriod/
ls
cat cluster.call.3.mouse.ReRun.sh.e31081451 
ls
cat cluster.call.3.mouse.ReRun.sh.o31081451 
ls analysisReRun/
ls analysisReRun/final/
pwd
cd analysisReRun/
pwd
cd final/
pwd
ls
cd ../../
ls
cat cluster.call.3.mouse.ReRun.sh.e31081451 
cat home/people/leolun/scripts/pipeline.no.trim.V3/fastqc_script.sh
cat /home/people/leolun/scripts/pipeline.no.trim.V3/fastqc_script.sh
head scripts/cluster.call.3.mouse.ReRun.sh 
cat cluster.call.3.mouse.ReRun.sh.e31081451 
exit
module load tools gcc/8.2.0 intel/perflibs/2018
module load T
module load R/4.0.0
R
nano jackard.sh
nano jackard.R
Rscript jackard.R
nano xRes
ls
rm core.*
mv jackard ./projects/ontologyDescent/
mv jackard.R ./projects/ontologyDescent/
ls
ls R/
cd projects/ontologyDescent/
ls
nano jackard.R 
Rscript jackard.R 
nano jackard.R 
Rscript jackard.R 
nano jackard.R 
q
Rscript jackard.R
nano jackard.R 
pwd
nano jackard.R 
Rscript jackard.R
ls
rm jackard.R~
rm xDist.Rdata 
Rscript jackard.R
ls
nano jackard.sh
module load tools gcc/10.2.0 intel/perflibs/2019 R/4.0.0
nano jackard.sh
pwd
Rscript /home/people/leolun/projects/ontologyDescent
Rscript /home/people/leolun/projects/ontologyDescent/jackard.R
ls
nano jackard.sh
nano jackard.R
submitJob 1 24:00:00 jackard.sh
cat ~/.bashrc 
submitJob 24:00:00 1 jackard.sh
qstat
ls
cat jackard.sh.e31084878 
ls
cat jackard.sh.o31084878 
nano jackard.R
submitJob 24:00:00 1 jackard.sh
qqasdasd
q
qstat
exit
qstat
cd projects/ontologyDescent/
ls
rm jackard.sh.*
ls
qstat
qstat
ls
projects/
cd projects/
ls
cd ontologyDescent/
ls
cat jackard.sh.e31084880 
ls
ll
ll -h
pwd
cd projects/
ls
cd ontologyDescent/
ls
rm computerome.Rdata 
module load tools gcc/8.2.0
module load intel/perflibs/2018
module load R/4.0.0
R
iqsub
cd projects/ontologyDescent/
ls
ll
nano jackard.R
nano ~/.bashrc 
submitJob 2:00:00 1 jackard.sh
iqsub
qstat
ls
nano jackard.R
module load gcc/8.2.0 intel/perflibs/2019
module load tools gcc/8.2.0 intel/perflibs/2019
module load R
module load R/4.0.0
R
top
nano jackard.R
submitJob 3:00:00 1 jackard.sh
qstat
mjobctl -F 31116763
qstat
mjobctl -C 31116763
qstat
ls
ll
rm jackard.sh.*
ls
submitJob 12:00:00 1 jackard.sh
qstat
ls
cat jackard.sh.e31116773 
cat jackard.sh.o31116773 
nano jackard.R
cd projects/201001_PhotoPeriod/
ls
cat cluster.call.3.mouse.ReRun.sh.e31081451 
head cluster.call.3.mouse.ReRun.sh
nano scripts/cluster.call.3.mouse.ReRun.sh 
module load module load FastQC/0.11.2
module load tools
module load FastQC/0.11.2
module load fastqc/0.11.8
nano scripts/cluster.call.3.mouse.ReRun.sh 
module load tools
module load ngs
module load gcc
module load perl/5.24.0
module load java/1.8.0-openjdk
module load fastqc/0.11.8
module load star/2.7.2b
module load subread/1.6.2
module load intel/perflibs
module load R/4.0.0
module load anaconda2/4.4.0
ls
head cluster.call.3.mouse.ReRun.sh.e31081451 
head cluster.call.3.mouse.ReRun.sh.o31081451 
tail cluster.call.3.mouse.ReRun.sh.o31081451 
ls
rm -r analysisReRun/
rm cluster.call.3.mouse.ReRun.sh.*
nano ~/.bashrc 
source ~/.bashrc 
nano ~/.bashrc 
source ~/.bashrc 
nano ~/.bashrc 
source ~/.bashrc 
nano ~/.bashrc 
source ~/.bashrc 
submitJob -h
nano ~/.bashrc 
submitJob -h
source ~/.bashrc 
submitJob -h
nano ~/.bashrc 
submitJob -h
source ~/.bashrc 
submitJob -h
source ~/.bashrc 
nano ~/.bashrc 
source ~/.bashrc 
submitJob -h
submitJob 1:00:00 40 190 ./scripts/cluster.call.3.mouse.ReRun.sh 
ls
qstat
cd projects/201001_PhotoPeriod/
ls
cat cluster.call.3.mouse.ReRun.sh.e31173570 
cd analysisReRun/final/
ls
cd ..
ls
cd final/
ls
pwd
cd projects/
cd ontologyDescent/
ls
module load tools gcc/8.2.0 R/4.0.0
module load tools gcc/8.2.0 intel/perflibs/64/2019 R/4.0.0
R
exit
cd projects/ontologyDescent/
ls
cd DistancesAndClustering/
ls
head ../clusters.sh
module load tools gcc/10.2.0 intel/perflibs/2019 R/4.0.0
R
lks
ls
nano cluster.R
cp ../clusters.sh ./
ls
mv cluster.R clusters.sh
ls
mv clusters.sh clusters.R
cp ../clusters.sh ./
ls
nano clusters.sh 
pwd
nano clusters.sh 
nano clusters.R
submitJob -h
submitJob 00:10:00 40 160 ./clusters.sh
cat clusters.sh
ls
ll -h
ls
ll -h
qstat
ls
cat clusters.sh.e31257528 
ls
rm clusters.sh.*
ls
cat clusters.R
ls /home/people/leolun/projects/ontologyDescent/DistancesAndClustering
nano clusters.R
submitJob 00:10:00 40 160 ./clusters.sh
qstat
ls
qstat
ls
nano clusters.R
qstat
submitJob 00:10:00 40 160 ./clusters.sh
qstat
ls
qstat
ls
cat clusters.sh.o31257531 
ll
head clusters.sh.o31257531 
head clusters.sh.e31257531 
tail clusters.sh.e31257531 
ls
ll
qstat
cat clusters.sh.o31257534 
cat clusters.R
cat clusters.sh.o31257534 
ll
nano clusters.R
submitJob 04:00:00 40 160 ./clusters.sh
cp clustersMCCLUST.sh
cp clusters.sh clustersMCCLUST.sh
nano clustersMCCLUST.sh 
nano clusters.R
ls
rm clusters.sh.*
ls
cat clustersMCCLUST.sh 
submitJob 24:00:00 40 160 ./clustersMCCLUST.sh
ll -h
qstat
ls
cat clusters.R
qstat
mjobctl -F 31257545
exit
iqsub 
cd projects/ontologyDescent/
ls
mkdir JackardDistances
mv * JackardDistances/
ls
ls JackardDistances/
mkdir DistancesAndClustering
cd DistancesAndClustering/
ls
pwd
qsub
iqsub 
ls
ll
ll -g
ll -h
ls
cd ..
ls
cat clusters.sh.o31256539 
cat clusters.sh.o31256434 
lsls
ls
cd DistancesAndClustering/
ls
crm parameterOptimization.Rdata 
rm parameterOptimization.Rdata 
ls
iqsub
mjobctl -F 31257545
mjobctl -F 31257546
ls
qstat
ls
rm clusters.sh.*
rm clustersMCCLUST.sh.*
ls
nano clusters.R 
submitJob -h
submitJob 04:00:00 40 180 ./clusters.sh 
nano clustersMCLUST.R 
cat clusters.R 
cat clustersMCLUST.R 
cat clusters.R 
qstat
ls
qstat
ls
setwd("/home/people/leolun/projects/ontologyDescent/DistancesAndClustering")
load("env.RData")
library(simplifyEnrichment)
library(igraph)
resOptimization <- NULL
#############
#Topology based diatance and all clustering methods.
resOptimization$TopoDist <- NULL
print("TopoDist")
print("----------------")
for(meth in c("fast_greedy", "louvain", "walktrap", "leading_eigen")){
  resOptimization$TopoDist[[meth]] <- mclapply(datClust[1:3], function(n){
    targets <- unique(unlist(n))
    test <- clustereR(net, GOnames, GOlength, targets, method = meth)$res
    test <- structure(test$clusterNumber, names = test$ontoID)
    test}, mc.cores = 3)
}
for(meth in c("kmeans", "dynamicTreeCut", "apcluster", "hdbscan", "MCL", "mclust")){#mclust is too slow even on the fucking cluster.
  resOptimization$TopoDist[[meth]] <- mclapply(datClust, function(n){
    #calculate distances
    targets <- unique(unlist(n))
    subNet <- shortest_paths(net, from = targets, to = targets, mode = "all")
    subNet <- subNet$vpath
    subNet <- unique(names(unlist(subNet)))
    subNet <- igraph::induced_subgraph(net, subNet)
    
    test <- cluster_terms(distances(subNet, targets, targets),
                          method = meth,
                          catch_error = T)
    test <- try(structure(as.numeric(test), names = targets))
    test
    
    test}, mc.cores = 40)
}
#############a
#All IC based distances
for(distance in c("Resnik", "Rel", "Jiang", "Lin", "Wang")){
  
  print(distance)
  print("----------------")
  
  resOptimization[[distance]] <- NULL
  
  resOptimization[[distance]] <- mclapply(datClust, function(n){
    n <- lapply(all_clustering_methods()[c(-1)], function(meth){
      targets <- unique(unlist(n))
      targetsDistance <- GO_similarity(targets, "MF", measure = distance)
      
      test <- cluster_terms(targetsDistance,
                            method = meth,
                            catch_error = T)
      test <- try(structure(as.numeric(test), names = targets))
      test
    })
    names(n) <- all_clustering_methods()[c(-1,-3)]
    n
  }, mc.cores = 40)
}
submitJob -h
submitJob 24:00:00 40 180 ./clustersMCCLUST.sh 
ls
qstat
ld
ls
cat clusters.sh.o31257973 
ll
ll -h
