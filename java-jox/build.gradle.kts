plugins {
    application
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(25))
    }
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("com.softwaremill.jox:structured:0.5.0")
    testImplementation("org.junit.jupiter:junit-jupiter:6.0.1")
    testImplementation("org.testcontainers:testcontainers:2.0.1")
    testImplementation("org.testcontainers:testcontainers-junit-jupiter:2.0.1")
    testRuntimeOnly("org.slf4j:slf4j-simple:2.0.17")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

application {
    mainClass = "Main"
}

tasks.withType<JavaCompile> {
    options.compilerArgs.addAll(listOf("--enable-preview"))
}

tasks.withType<JavaExec> {
    jvmArgs("--enable-preview")
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
    jvmArgs("--enable-preview")

    testLogging {
        showStandardStreams = true
        exceptionFormat = org.gradle.api.tasks.testing.logging.TestExceptionFormat.FULL
        events(org.gradle.api.tasks.testing.logging.TestLogEvent.STARTED, org.gradle.api.tasks.testing.logging.TestLogEvent.PASSED, org.gradle.api.tasks.testing.logging.TestLogEvent.SKIPPED, org.gradle.api.tasks.testing.logging.TestLogEvent.FAILED)
    }
}
