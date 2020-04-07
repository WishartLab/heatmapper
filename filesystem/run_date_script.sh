startdate=20200405
enddate=$(date +%Y%m%d)
loopdate=$startdate

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

cd ..

python convolute_regions.py

cd Global

python generate_regional_prediction.py