// This line loads the shared library with some global methods
library 'decisionbrain-utils'

// This is the main pipeline declaration : you should have only one in your file
pipeline {
    // The agent must be set to none because some steps run in docker, some not
    agent any

    triggers {
        pollSCM 'H/2 * * * *'
    }

    options {
        // Number of builds to keep
        buildDiscarder(logRotator(numToKeepStr: '15', artifactNumToKeepStr: '15'))

        disableConcurrentBuilds()
    }

    environment {
        // Get the credentials defined globally in jenkins.
        // This methods creates 2 useful variables : NEXUS_USER_USR that contains the user name and NEXUS_USR_PSW that contains the password
        NEXUS_USER = credentials('NEXUS_USER')
        BITBUCKET_USER = credentials('BITBUCKET_USER')

        // This variable will be used as default value is the PROJECT_VERSION can't be extracted
        PROJECT_DEFAULT_VERSION = "1.0.0-SNAPSHOT"
    }

    stages {
        // If you do not defined your project in Jenkins DecisionBrain folder, you should have a json file in your job's "Config files" with the id variables-config
        // This should look like :
        // {
        //   nexusUrl:'https://nexus.decisionbrain.loc/',
        //   dockerRegistry:'https://docker-registry.decisionbrain.loc',
        //   sonarUrl:'https://sonar.decisionbrain.loc/',
        //   bitbucketUrl: 'https://api.bitbucket.org/2.0/repositories/decisionbrain'
        // }
        stage('Config file reading') {
            agent any

            steps {
                // Reads the variables-config file and extract the variables
                configFileProvider([configFile(fileId: 'config-file', variable: 'VARIABLES')]) {
                    script {
                        // Read the Json config file and sets the corresponding environment variables
                        // This method is defined in the DecisionBrain shared library
                        readConfigFile()
                    }
                }
            }
        }

        // This stage checkouts the branch and defines the PROJECT_VERSION variable
        stage('Checkout source code') {
            agent any
            steps {
                echo "Pulling : " + BRANCH_NAME
                checkout scm

                script {
                    // Get the project version and sets the PROJECT_VERSION variable
                    // This method is defined in the DecisionBrain shared library
                    getProjectVersion()
                }
            }
        }
       stage('Simple build') {
            when { not { anyOf { branch 'master'; branch 'develop' } } }
            // This agent is run inside a container which is defined by default by the Dockerfile in the root directory
            // If the dockerfile you want to use is different, please specify the following property : filename 'yourDockerFilePath'
            agent {
                dockerfile {
                    filename 'Dockerfile.build'
                    reuseNode true
                    // Needed to work inside the docker environment and get access to the nexus.loc
                    args "--network=dockernet254"
                }
            }

            steps {
                sh "chmod ugo+x ./gradlew"
                sh "chmod -R ugo+rx ./gradle"
                sh "./gradlew clean build -P NEXUS_URL=$NEXUS_URL -P MAVEN_USER=$NEXUS_USER_USR -P MAVEN_PASSWORD=$NEXUS_USER_PSW"
            }
        }
        // This stage is only run on develop branch
        stage('Develop build with sonar') {
            when { anyOf { branch 'develop' } }
            agent {
                dockerfile {
                        filename 'Dockerfile.build'
                        reuseNode true
                        args "--network=dockernet254"
                }
            }
            steps {
                sh "chmod ugo+x ./gradlew"
                sh "chmod -R ugo+rx ./gradle"
//                sh "./gradlew clean build jacocoTestReport pitest jacocoMergeTest sonarqube publish -P NEXUS_URL=$NEXUS_URL -P MAVEN_USER=$NEXUS_USER_USR -P MAVEN_PASSWORD=$NEXUS_USER_PSW -Dsonar.host.url=$SONAR_URL"
                sh "./gradlew clean build publish -P NEXUS_URL=$NEXUS_URL -P MAVEN_USER=$NEXUS_USER_USR -P MAVEN_PASSWORD=$NEXUS_USER_PSW -Dsonar.host.url=$SONAR_URL"
            }
        }
        // This stage is only run on master branch
        stage('Master build') {
            when { anyOf { branch 'master' } }
            agent {
                dockerfile {
                    filename 'Dockerfile.build'
                    reuseNode true
                    args "--network=dockernet254"
                }
            }
            steps {
                sh "./gradlew clean build publish -P NEXUS_URL=$NEXUS_URL -P MAVEN_USER=$NEXUS_USER_USR -P MAVEN_PASSWORD=$NEXUS_USER_PSW"
                sh "./gradlew publish -P NEXUS_URL=$NEXUS_URL -P NEXUS_URL_FOR_PUSH=$NEXUS_DMZ_URL -P MAVEN_USER=$NEXUS_DMZ_USER_USR -P MAVEN_PASSWORD=$NEXUS_DMZ_USER_PSW"
            }
        }
    }

    post {
        always {
//            junit testResults: '**/build/test-results/test/*.xml', allowEmptyResults: true

            script {
                // Send the build status as a comment inside the pull request, if any
                // This method is defined in the DecisionBrain shared library
                sendBuildStatus(project : 'decisionbrain-cplex-scala')
            }
        }
    }
}