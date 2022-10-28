FROM openfaas/classic-watchdog:0.20.1 as watchdog
FROM mcr.microsoft.com/dotnet/sdk:5.0 as builder

ENV DOTNET_CLI_TELEMETRY_OPTOUT 1

# Optimize for Docker builder caching by adding projects first.

RUN mkdir -p /home/app/src/binance-finance
WORKDIR /home/app/src/binance-finance
COPY ./binance-finance/Function.fsproj .

WORKDIR /home/app/src
COPY ./binance-finance.sln .
RUN dotnet restore

COPY .  .

RUN dotnet publish -c release -o published

FROM mcr.microsoft.com/dotnet/core/runtime:3.1

COPY --from=watchdog /fwatchdog /usr/bin/fwatchdog
RUN chmod +x /usr/bin/fwatchdog

# Create a non-root user
RUN addgroup --system app \
    && adduser --system --ingroup app app

WORKDIR /home/app/
COPY --from=builder /home/app/src/published .
RUN chown app:app -R /home/app

USER app

ENV fprocess="dotnet ./root.dll"
EXPOSE 8080

HEALTHCHECK --interval=3s CMD [ -e /tmp/.lock ] || exit 1

CMD ["fwatchdog"]
