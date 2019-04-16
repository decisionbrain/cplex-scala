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
        // For this step, you should have a json file in your job's "Config files" with the id variables-config
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
        stage('Build project') {
            // This agent is run inside a container which is defined by default by the Dockerfile in the root directory
            // If the dockerfile you want to use is different, please specify the following property : filename 'yourDockerFilePath'
            agent {
                dockerfile {
                    reuseNode true
                    // Needed to work inside the docker environment and get access to the nexus.loc
                    args "--network=dockernet254"
                    dir 'jenkins'
                }
            }
            steps {
                sh "./gradlew clean build sonarqube -P NEXUS_URL=$NEXUS_URL -P MAVEN_USER=$NEXUS_USER_USR -P MAVEN_PASSWORD=$NEXUS_USER_PSW -Dsonar.host.url=$SONAR_URL"
            }
        }
        // This stage builds and pushes the current project version images on master and develop branches
        stage('Publish Docker images') {
            // The docker build must NOT be run inside a container
            agent none

            steps {
                script {
                    echo "Building docker images"

                    docker.withRegistry(env.DOCKER_REGISTRY_URL, 'NEXUS_USER') {
                        /* Build images */
                        def frontendImage = docker.build("microservices-blueprint-frontend:${env.PROJECT_VERSION}", "frontend")
                        def openDataImage = docker.build("microservices-blueprint-opendata:${env.PROJECT_VERSION}", "--build-arg JAR_FILE=build/libs/microservices-blueprint-opendata-${env.PROJECT_VERSION}.jar opendata")

                        /* Push images to registry */
                        frontendImage.push()
                        openDataImage.push()

                        /* Clean local Docker cache */
                        sh "docker images microservices-blueprint-frontend:${env.PROJECT_VERSION} -q | xargs docker rmi -f"
                        sh "docker images microservices-blueprint-opendata:${env.PROJECT_VERSION} -q | xargs docker rmi -f"
                    }

                }
            }
        }
    }

    post {
        always {
            junit testResults: '**/build/test-results/test/*.xml', allowEmptyResults: true

            script {
                // Send the build status as a comment inside the pull request, if any
                // This method is defined in the DecisionBrain shared library
                sendBuildStatus(project : 'microservices-blueprint')
            }
        }
    }
}