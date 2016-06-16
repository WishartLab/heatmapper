
# Log user activity to a file, for use by the Heatmapper Monitor API used for
# directing users to appropriate server nodes. Note that if an activity string starts
# with 'begin', it will indicate to the API that a process has begun that may take an
# extended amount of time (more than a fraction of a second), so this node can be
# avoided if another user wants to use the same app.
#
# app: string identifying the Heatmapper app (e.g. 'geomap')
log_activity <- function(app, activity) {
	log.file = paste("/apps/heatmapper_monitor/heatmapper_logs/", app ,"_activity.log", sep="");
	z <- Sys.time();
	tryCatch({
		suppressWarnings(write(paste(unclass(z), z, activity, sep="\t"), file=log.file, append=TRUE));
			# unclass(z) will be the time in seconds since the beginning of 1970.
			# z will be printed as the human-readable date and time.
		}, 
	error = function(err){
		# May get error if path to log file not found, as when not running on production
		# or test server. As a hackish way of not printing error messages on one's local
		# development machine (assumed to be non-Linux), check the OS and only print an
		# error message if on Linux (assumed to be the production/test server OS).
		#
		# In case the log file may not be accessible occasionally, we just want to catch
		# the error and print a message, not prevent Heatmapper from performing an
		# operation. If any activity log file is inaccessible for a longer period, we
		# will let the Heatmapper Monitor app check and warn about that.
		if (Sys.info()[['sysname']] == 'Linux') {
			print(paste("ERROR: Could not write to activity log file", log.file))
		}
	})
}
