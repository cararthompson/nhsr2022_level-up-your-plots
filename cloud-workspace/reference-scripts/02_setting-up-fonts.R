# Set the graphics device to AGG in the Global Options and install {ragg} ----
# This should ensure fonts play nicely within the Viewer and any outputs

# Check which fonts are already installed ----
systemfonts::system_fonts() %>% View("fonts")

# Register a font variant ----
# Here we register the Ultrabold version of Lato
systemfonts::register_variant("Lato Megabold", # Pick a name, any name!
                              family = "Lato",
                              weight = "ultrabold")

# Registering a new font within the project ----
# First, upload the font file to the project using the "Files" tab.
# Then call systemfonts::register_font() as in the example below
systemfonts::register_font(name = "Poppins", 
                           # This is the path to the font I have already uploaded
                           plain = "fonts/Poppins-Bold.ttf")

# To check which fonts you have registered ----
systemfonts::registry_fonts()
