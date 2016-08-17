defmodule Prometheus.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus,
     version: "2.0.0",
     description: description,
     package: package]
  end

  defp description do
    """
    Prometheus monitoring system and time series database client in Erlang.
    """
  end

  defp package do
    [build_tools: ["rebar3"],
     maintainers: ["Ilya Khaprov"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/deadtrickster/prometheus.erl",
              "Elixir Plugs" => "https://hex.pm/packages/prometheus_plugs",
              "Ecto Collector" => "https://hex.pm/packages/prometheus_ecto"},
     files: ["src", "include", "README.md", "LICENSE", "rebar.config"]]
  end
end
