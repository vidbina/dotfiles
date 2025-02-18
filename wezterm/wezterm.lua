-- Tangled from README.org
local wezterm = require 'wezterm'
local config = {}

-- wezterm.gui is not available to the mux server, so take care to
-- do something reasonable when this config is evaluated by the mux
function get_appearance()
  if wezterm.gui then
    return wezterm.gui.get_appearance()
  end
  return 'Dark'
end

function scheme_for_appearance(appearance)
  if appearance:find 'Dark' then
    return 'Google Dark (Gogh)'
  else
    return 'Google Light (base16)'
  end
end

config.color_scheme = scheme_for_appearance(get_appearance())
-- workaround to tackle WezTerm issue
config.front_end = 'WebGpu'

return config
