# Make statistics on hough results
import statistics

def getPlanForEachPlan(db):
	data_per_plan = []
	for n in range(1,5):
		tmp_pos = open("Results/Hough_Benchmarks/DB" + str(db) + "/Plan" + str(n) + "/benchmark-hough-pos.txt")
		tmp_rand = open("Results/Hough_Benchmarks/DB" + str(db) + "/Plan" + str(n) + "/benchmark-hough-rand.txt")
		pos_list = tmp_pos.readlines()
		rand_list = tmp_rand.readlines()
		ret_pos = []
		ret_rand = []
		for i in range(len(pos_list)):
			ret_pos.append(float(pos_list[i][:-1]))
			ret_rand.append(float(rand_list[i][:-1]))
		data_per_plan.append((ret_pos,ret_rand))
	return data_per_plan

def countFalsePos(value,liste):
	ret = 0
	for i in liste:
		if i >= value: ret += 1
	return ret
 
def makeStatsPerPlan(function,db):
	plan_data = getPlanForEachPlan(db)
	db_results = []
	for plan in range(0,4):
		cur_pos = plan_data[plan][0]
		cur_rand = plan_data[plan][1]
		goal_threshold = function(cur_pos)
		db_results.append(countFalsePos(goal_threshold,cur_rand))
	return db_results

def foundBestResults(db):
	tries = [statistics.mean,
					statistics.harmonic_mean,
					statistics.median,
					statistics.median_low,
					statistics.median_high,
					statistics.median_grouped]
	cur_best_sum = sum(makeStatsPerPlan(tries[0],db))
	cur_best = makeStatsPerPlan(tries[0],db)
	for i in range(1,6):
		if sum(makeStatsPerPlan(tries[0],db)) < cur_best_sum:
			cur_best_sum = sum(makeStatsPerPlan(tries[0],db))
			cur_best = makeStatsPerPlan(tries[0],db)
	return cur_best

def getFinalResultsPerDB():
	for db in range(1,5):
		print("False positives for DB" + str(db) + " are " + str(foundBestResults(db)))
	
getFinalResultsPerDB()		
