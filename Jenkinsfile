pipeline {
    agent none
    options {
        parallelsAlwaysFailFast()
    }
    stages {
        stage ('Matrix') {
            matrix {
                agent {
                    label "${SYSTEM}"
                }
                axes {
                    axis {
                        name 'SYSTEM'
                        values 'x86_64-linux'
                    }
                }
                stages {
                    stage ('Cachix setup') {
                        steps {
                            cachixUse "nammayatri"
                        }
                    }
                    stage ('Nix Build All') {
                        steps {
                            nixCI system: env.SYSTEM
                        }
                    }
                    // stage ('Cachix push') {
                    //     when {
                    //         anyOf {
                    //             branch 'main';
                    //         }
                    //     }
                    //     steps {
                    //         cachixPush "nammayatri"
                    //     }
                    // }
                }
            }
        }
    }
}
