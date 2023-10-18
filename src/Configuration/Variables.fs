namespace Configuration

module Variables =
    open FsConfig

    type LoggingLevel =
    | Debug
    | Information
    | Warning
    | Error
    | Fatal

    [<Convention("PERPLEX")>]
    type ConfigurationVariables = {
        [<CustomName("STATS")>]
        LoggingLevel: LoggingLevel
        [<DefaultValue("/tmp/perplexdb/logs")>]
        LoggingPath: string
    }