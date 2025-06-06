def makeButtons(sites, generalFormat, identifier):
    buttons = ""
    scripts = f"""
                    <script>
                    function toggle{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        if (!document.fullscreenElement) {{
                            imgElement.requestFullscreen();
                        }} else {{
                            document.exitFullscreen();
                        }}
                    }}
                    </script>
    """
    inputDir = "https://raw.githubusercontent.com/CI-CMG/SoundscapesWebsite/refs/heads/main/content/resources"
    path = f'{inputDir}/{generalFormat}'
    path = path.replace("***", sites[0])
    initialImage = f'<img src="{path}" width="700" id="{identifier}" onclick="this.requestFullscreen()">'
    if len(sites) == 1:
        return initialImage + scripts
    
    for site in sites:
        path = f'{inputDir}/{generalFormat}'
        path = path.replace("***", site)

        buttons += f'<button onclick="{site}{identifier}()" style="padding: 10px; color: white; margin: 4px 4px; background-color: #008CBA;">{site}</button>'
        scripts += f"""
                    <script>
                    function {site}{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        imgElement.src = "{path}";
                    }}
                    </script>
        """
    return buttons + initialImage + scripts
  
def makeImage(imageName, identifier):
    inputDir = "https://raw.githubusercontent.com/CI-CMG/SoundscapesWebsite/refs/heads/main/content/resources"
    path = f'{inputDir}/{imageName}'
    initialImage = f'<img src="{path}" width="700" id="{identifier}" onclick="toggle{identifier}()">'
    initialImage += f"""
                    <script>
                    function toggle{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        if (!document.fullscreenElement) {{
                            imgElement.requestFullscreen();
                        }} else {{
                            document.exitFullscreen();
                        }}
                    }}
                    </script>
    """
    return initialImage
  
def embedMapViewer(srcLink):
    return f'<embed src="{srcLink}" style="width:900px; height: 800px;">'