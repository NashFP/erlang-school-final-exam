defmodule Step1.Mixfile do
  use Mix.Project

  def project do
    [ app: :step1,
      version: "0.0.1",
      elixir: "~> 0.10.3-dev",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [
      applications: [:gproc]
    ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [
      { :gproc, github: "uwiger/gproc" }
    ]
  end
end
