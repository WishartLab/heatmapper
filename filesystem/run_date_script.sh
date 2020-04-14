enddate=$(date +%Y%m%d)
loopdate=$startdate

cd COVID-19
git pull
cd ..

cd ..
git pull
cd filesystem/

let j=0
while [ "$loopdate" -ne "$enddate" ]; do
        loopdate=`date   -j -v+${j}d  -f "%Y%m%d" "$startdate" +"%Y%m%d"`
        python daily_task.py $loopdate
        let j=j+1
done


cd ..
git add -a filesystem/
git commit -m "Automated Data Generation $startdate"
git push

cd Global

python modify_total_files.py

cd ..

python convolute_regions.py

cd Global

python generate_future_files.py

# cd heatmapper-docker/web/heatmapper
# # check if we are in correct branch
# git branch
# #Should be covid-19, pull all the changes
# git pull
# # move back to heatmapper-docker directory
# cd ..
# cd ..
# #tag last working container with date
# sudo docker tag heatmapper:latest heatmapper:date
# #build containers
# sudo docker-compose build
# # bring old containers down
# sudo docker-compose rm -sf
# # push new containers live
# sudo docker-compose up -d
