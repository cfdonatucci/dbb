def zosAgent   = 'zPDT'
pipeline {
    agent { label zosAgent }
    stages {
// -------------------------------------------------------------------------        
		stage('BR14') {
			steps {
				script{
					node( 'mainframe' ) {
					    echo 'execute BR14'
					    sh "/var/dbb/bin/groovyz /u/adcdmst/scripts/iefbr14.groovy"  
					    
 					}
				}
		     }
    	}
// -------------------------------------------------------------------------
		stage('Iebcopy') {
			steps {
				script{
					node( 'mainframe' ) {
					    echo 'execute Iebcopy'
					    sh "/var/dbb/bin/groovyz /u/adcdmst/scripts/iebcopy.groovy" 
 					}
				}
		     }
    	}
// -------------------------------------------------------------------------

	}
}
