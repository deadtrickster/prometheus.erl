defmodule Prometheus.Mixfile do
  use Mix.Project

  def project do
    [app: :prometheus,
     version: "1.5.0",
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
     links: %{"GitHub" => "https://github.com/deadtrickster/prometheus.erl"},
     files: ["src", "include", "README.md", "LICENSE", "rebar.config"]]
  end
end
