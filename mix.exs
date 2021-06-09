defmodule Prometheus.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus,
     version: "4.8.1",
     description: description(),
     package: package(),
     deps: deps()]
  end

  defp description do
    """
    Prometheus monitoring system and time series database client in Erlang.
    """
  end

  def application do
    [mod: { :prometheus, [] }]
  end

  defp package do
    [build_tools: ["rebar3", "mix"],
     maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"Cowboy exporter/instrumenter" => "https://hex.pm/packages/prometheus_cowboy",
              "Ecto Instrumenter" => "https://hex.pm/packages/prometheus_ecto",
              "GitHub" => "https://github.com/deadtrickster/prometheus.erl",
              "Inets HTTPD Exporter" => "https://hex.pm/packages/prometheus_httpd",
              "Phoenix Instrumenter" => "https://hex.pm/packages/prometheus_phoenix",
              "Plugs Instrumenter/Exporter" => "https://hex.pm/packages/prometheus_plugs",
              "Process info Collector" => "https://hex.pm/packages/prometheus_process_collector",
              "Prometheus.ex" => "https://hex.pm/packages/prometheus_ex"},
     files: ["mix.exs", "bin", "src", "include", "README.md", "LICENSE", "rebar.config"]]
  end

  defp deps do
    [
      {:quantile_estimator, "~> 0.2.1"}
    ]
  end
end
