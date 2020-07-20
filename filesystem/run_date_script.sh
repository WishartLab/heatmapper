startdate="20200718"
enddate=$(date +%Y%m%d)
loopdate=$startdate
git pull
cd COVID-19
git pull
cd ..

let j=0
while [ "$loopdate" -ne "$enddate" ]; do
        loopdate=`date   -j -v+${j}d  -f "%Y%m%d" "$startdate" +"%Y%m%d"`
        python daily_task.py $loopdate
        let j=j+1
done

cd Global

python modify_total_files.py

python convolute_regions.py

python generate_future_files.py

cd ..
cd ..

git add filesystem/
git commit -m "Automated prediction script"
git push

ssh ubuntu@52.2.187.145 'cd heatmapper-docker/web/heatmapper && git branch && git pull && cd .. && cd .. && yes "y" | sudo docker system prune && sudo docker-compose build && sudo docker-compose rm -sf && sudo docker-compose up -d'


# # # tar -zcvf Downloads/Best_Case.tar Best_Case
# # tar -zcvf Downloads/Worst_Case.tar Worst_Case
# 


# # # # # # #go to heatmapper directory
# ssh ubuntu@52.2.187.145 
# cd heatmapper-docker/web/heatmapper
# # check if we are in correct branch
# git branch
# #Should be covid-19, pull all the changes
# git pull
# # # move back to heatmapper-docker directory
# cd ..
# cd ..
# # #tag last working container with date
# sudo docker tag heatmapper:latest heatmapper:date
# #build containers
# yes "y" | sudo docker system prune
# sudo docker-compose build
# # bring old containers down
# sudo docker-compose rm -sf
# # push new containers live
# yes "" | sudo docker-compose up -d
# # check if everything is working, If yes remove last container
# # 1. Check the image id
# sudo docker images
# # # #2. remove image by image ID
# # # sudo docker rmi image_id