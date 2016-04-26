defmodule Consolex.Mixfile do
  use Mix.Project

  def project do
    [app: :consolex,
     version: "0.0.3",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     description: "An IEx web console",
     package: package,
     deps: deps
   ]
  end

  def package do
    [
      maintainers: ["Sushruth Sivaramakrishnan (sivsushruth)"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/sivsushruth/consolex"}
    ]
  end

  def application do
    [
      mod: {Consolex.Server, []},
      applications: [:logger, :ranch, :cowboy, :porcelain]
    ]
  end

  defp deps do
    [
      {:cowboy, "~> 1.0"},
      {:exjsx, "~> 3.2"},
      {:porcelain, "~> 2.0"}
    ]
  end
end
