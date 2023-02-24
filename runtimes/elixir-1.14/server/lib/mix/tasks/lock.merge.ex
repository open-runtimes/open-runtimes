defmodule Mix.Tasks.Lock.Merge do
  @moduledoc "The config.merge mix task: `mix help config.merge`"
  use Mix.Task

  @user_lock_path Path.join(["/", "usr", "code", "mix.lock"])

  @shortdoc "Merge user with app config"
  def run(_) do
    # IO.inspect Mix.Project.config()[:lockfile]
    # IO.inspect @user_lock_path

    if File.exists?(@user_lock_path) && File.regular?(@user_lock_path) do
      usr_lock = Mix.Dep.Lock.read()
      srv_lock = Mix.Dep.Lock.read(@user_lock_path)
      new_lock = Map.merge(srv_lock, usr_lock)
      Mix.Dep.Lock.write(new_lock)
    end
  end
end
