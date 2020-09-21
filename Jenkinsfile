// --> Simplified jenkinsfile using master branch
//
// WORKSPACE es /u/carlos ..
//
println "** Pipeline script embedded"
// ---------------------------Agents labels
def linuxAgent = 'principal'
def zosAgent   = 'itso'
// ---------------------------Verbose
def buildVerbose = 'false'
// ---------------------------Hosts and ports
def linuxHost = ''
def zosHost = 'wtsc80.cpolab.ibm.com'
def zosPort = '22'
// ----------------------------DBB https
def dbbUrl = 'https://'+linuxHost+':9080/dbb'  
def dbbHlq = 'CARLOS.DBB'
def dbbDaemonPort = ''
def dbbGroovyzOpts= ''
// ---------------------------- Git (GitHub)
def gitCredId  = 'cfdonatucci'
def gitCred    = 'cfdonatucci'
def gitOrg     = 'cfdonatucci'
def gitHost    = 'github.ibm.com'
def srcGitRepo = 'git@'+gitHost+':'+gitOrg+'/dbb.git'
def srcGitBranch = '*dev*'
//def srcGitRepo   = 'git@github.com:cfdonatucci/dbb.git'
// ----------------------------- Build type
//  -i: incremental
//  -f: full
def buildType='-f'
// ----------------------------  Build extra args
//  -d: COBOL debug options
def buildExtraParams='-d'
// ===========================================================
pipeline { 
  agent { label zosAgent }
      environment { WORK_DIR = "${WORKSPACE}/builds/build-${BUILD_NUMBER}" }
      options { skipDefaultCheckout(true) }
// -------------------------------------------------------------------------
  stages { 
    stage('Init') {
     steps {
      script {
        env.DBB_HOME = '/var/dbb/'
       	echo "WorkSpace : ${WORKSPACE}/builds/build-${BUILD_NUMBER}"
       	echo "Repository: ${srcGitRepo} - branch: ${srcGitBranch} "
        }
       }
      }
// -------------------------------------------------------------------------
    stage('Git Clone/Refresh') {
        agent { label zosAgent }
        steps {
            script {
                println "** >branch: ${srcGitBranch}"
                println "** >WORKSPACE  is ${WORKSPACE}"
//
                dir('dbb-zAppBuild') {
                checkout([$class: 'GitSCM', branches: [[name: srcGitBranch]], doGenerateSubmoduleConfigurations: false,
                  submoduleCfg: [], userRemoteConfigs: [[url: srcGitRepo]]])
              } } } }
// -----------------------------------ws /u/carlos/jenkins --------------------------------------
		stage('DBB Build') {
			steps {
				script{
					node( zosAgent ) {
						
sh "$DBB_HOME/bin/groovyz /u/carlos/dbb-zAppBuild/build.groovy $buildType -w ${WORKSPACE} -o ${WORKSPACE} -a dbb-zAppBuild/appName -h ${dbbHlq}.STG "

	
 					}
				}
			}
		}
  }//pipeline
}//stages
