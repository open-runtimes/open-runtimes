package io.openruntimes.kotlin

class RuntimeContext(val req: RuntimeRequest, val res: RuntimeResponse, val logger: RuntimeLogger) {
    fun log(message: Any) {
        this.logger.write(message + "\n", RuntimeLogger.TYPE_LOG)
    }

    fun error(message: Any) {
        this.logger.write(message + "\n", RuntimeLogger.TYPE_ERROR)
    }
}

