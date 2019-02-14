# this reads MTurk logs from the MOT project and produces two files
#demo.csv and trials.csv
#demo.csv will be demographics, such as each subject's debriefing answers, velocity, age, gender.
#trials.csv will be the a log of all trials that subjects did

# Make sure to move or delete the previousely saved demo.scv and trials.csv before running this script 

from os import listdir
from os.path import isfile, join


def duration_min(start, end):
    t = start.split(":")
    e = end.split(":")
    s = float(t[0])*60*60*1000 + float(t[1])*60*1000 + float(t[2])
    f = float(e[0])*60*60*1000 + float(e[1])*60*1000 + float(e[2])
    return round(( f-s )/60/1000)
    
main_path = '/Users/luckyfish/Desktop/MOT/MOTReplications/TurkLogs/MOTFeb12/'          # path to where all the logs are
logfiles = [f for f in listdir(main_path) if isfile(join(main_path, f))]      # list of all logs

demof = open("/Users/luckyfish/Desktop/MOT/MOTReplications/MOTFeb12demo.csv", "w")            # here will be demo.csv
demof.write("ip\tbrowser\tdate\tstart\tend\tsubject\tvelocity\taccstable\taccunstable\tnumstable\t")
demof.write("numunstable\tage\tgender\tmonitor\twhattesting\thowwell\tstrategies\tbackground\tbkgdiff\tunstable\tproblems\tcalibw\tcalibh\tdurationMOT\tdurationExperiment\n")
demof.flush()

trialsf = open("/Users/luckyfish/Desktop/MOT/MOTReplications/MOTFeb12data.csv", "w")        # here will be trials.csv
trialsf.write("subject\ttrial\tcondition\tbkgid\tnumcorrect\trt1\trt2\trt3\trt4\n")
trialsf.flush()


for logf in logfiles :
    if (logf != '.DS_Store'):                                                 # there will be a file called .DS_Store, which we ignore
        print ("processing", logf)

        f = open(main_path + logf, "r")                                       # open this subject's log
        
        line = f.readline()                                                   # read the first line
        oldline = line;
        
        fields = line.split(" ")                                              # the fields are separated by spaces
                                                                              # now, extract demographic info for this subject
        ip = fields[0]                                                        # ip address
        date = fields[1]                                                      # experiment date
        start = fields[2]                                                     # a timestamp for when one started the experiment
        browser = fields[3]
        subject = fields[4]                                                   # subject ID

        line = f.readline()                                                   # read the second line, which has calibration velocity
        if (oldline == line):
            line = f.readline()
            
        oldline = line;
        
        fields = line.split(" ")
        velocity = fields[5];                                                 # velocity, in the form 'calibrationvelocity:2.12'
        print("velocity:", velocity)
        velocity = velocity.split(":")[1]                                     # we only want the actual number

        # on Jan 14: there are two more fields in this line calibw:1920 calibh:938
        if (len(fields) > 7):
            calibw = fields[7]
            calibw = calibw.split(":")[1]
            calibh = fields[8];
            calibh = calibh.split(":")[1]
            calibh = calibh[0:len(calibh)-1]                                          # there is an \n in the end
        

        startMOT = fields[2]
        
        line = f.readline()                                                   # read the third line, experiment log
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        fields = line.split(" ")
        accstable = fields[5]                                                 # fraction of trials correct in the stable condition
        accstable = accstable.split(":")[1]
        accunstable = fields[6]                                               # fraction of trials correct in the unstable condition
        accunstable = accunstable.split(":")[1]
        objstable = fields[7]                                                 # fraction of objects correct in the stable condition
        objstable = objstable.split(":")[1]
        objunstable = fields[8]                                               # fraction of objects correct in the unstable condition
        objunstable = objunstable.split(":")[1]
        numcorrectstable = fields[9]                                          # number of objects correct in the stable condition
        numcorrectstable = numcorrectstable.split(":")[1]
        numcorrectunstable = fields[10]                                       # number of objects correct in the unstable condition
        numcorrectunstable = numcorrectunstable.split(":")[1]
        resplog = fields[11]                                                  # parsing the log of all responses separately below

        endMOT = fields[2]
        
        # on Jan 14: there are two more fields in this line testw:1536 testh:750

        line = f.readline()                                                   # read the next line, debriefing
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        fields = line.split(" ")
        
        age = fields[5]
        age = age.split(":")[1]
        gender = fields[6]                                                    # we only ned the first letter of gender, uppercased 
        gender = gender.split(":")[1]
        gender = gender[0].upper()
        monitor = line[line.find("monitor:") + len("monitor:"):len(line)-1]   # the monitor tag will have spaces in it, so we can not use split anymore

        line = f.readline()                                                   # various other debriefing questions
        if (len(line) == 0):  continue 
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        whattesting = line[line.find("whattesting:") + len("whattesting:"):len(line)-1]

        line = f.readline()
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        howwell = line[line.find("howwell:") + len("howwell:"):len(line)-1]

        line = f.readline()
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        strategies = line[line.find("strategies:") + len("strategies:"):len(line)-1]

        line = f.readline()
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        background = line[line.find("background:") + len("background:"):len(line)-1]

        line = f.readline()
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        bkgdiff = line[line.find("bkgdiff:") + len("bkgdiff:"):len(line)-1]

        line = f.readline()
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        unstable = line[line.find("unstable:") + len("unstable:"):len(line)-1]

        line = f.readline()
        if (oldline == line):
            line = f.readline()
            print ("dublicate line")
            
        oldline = line;
        
        problems = line[line.find("problems:") + len("problems:"):len(line)-1]
        end = line.split(" ")[2]                                              # a timestamp for when one ended the experiment
        
        demof.write(ip + "\t" + browser + "\t" + date + "\t" + start + "\t" + end + "\t" + subject + "\t" + velocity + "\t" + accstable + "\t" + accunstable + "\t")
        demof.write(numcorrectstable + "\t" + numcorrectunstable + "\t" + age + "\t" + gender + "\t" + monitor + "\t" + whattesting + "\t")
        demof.write(howwell + "\t" + strategies + "\t" + background + "\t" + bkgdiff + "\t" + unstable + "\t" + problems + "\t")
        demof.write(calibw  + "\t" + calibh + "\t" + str(duration_min(startMOT, endMOT)) + "\t" + str(duration_min(start, end)) + "\n")
        demof.flush()

        
        resplog = resplog[len("resplog:") + 1:len(resplog)-2]                # the second part, parsing the resplog into trials.csv; remove the trailing "(" and ")"
        resplog = resplog.split(")(")                                        # now resplog is an array of trial responses
        for log in resplog:                                                  # loop through the array and write each line to trails.csv
            ls = log.split(",");
            trialsf.write(subject + "\t" + ls[0] + "\t" + ls[1] + "\t" + ls[2] + "\t" + ls[3] + "\t" + ls[4] + "\t" + ls[5] + "\t" + ls[6] + "\t" + ls[7] + "\n")
                                                                             




demof.close()
trialsf.close()


