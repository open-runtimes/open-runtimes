package io.openruntimes.kotlin

class RuntimeContext(
    val req: RuntimeRequest,
    val res: RuntimeResponse,
    val logger: RuntimeLogger,
) {
    fun log(message: Any) {
        this.logger.write(message, RuntimeLogger.TYPE_LOG)
        this.logger.write("\n", RuntimeLogger.TYPE_LOG)
    }

    fun error(message: Any) {
        this.logger.write(message, RuntimeLogger.TYPE_ERROR)
        this.logger.write("\n", RuntimeLogger.TYPE_ERROR)
    }
}
