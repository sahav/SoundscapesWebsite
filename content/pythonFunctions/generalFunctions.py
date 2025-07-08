import os

def makeButtonsFit(sites, generalFormat, identifier):
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
        initialFile = generalFormat.replace('***', site, 1)
        parts = initialFile.split('***')
        cwd = os.getcwd()
        print(cwd)
        directory_path = cwd.split("SoundscapesWebsite")[0]+ "SoundscapesWebsite/SoundscapesWebsite/content/resources"
        
        fullFileName = ""
        for root, dirs, files in os.walk(directory_path):
            for file in files:
                if file.startswith(parts[0]) and file.endswith(parts[1]):
                    fullFileName = file
        
        path = f'{inputDir}/{fullFileName}'

        buttons += f'<button onclick="{site}{identifier}()" style="padding: 10px; color: white; margin: 4px 4px; background-color: #008CBA;text-transform: uppercase;">{site}</button>'
        scripts += f"""
                    <script>
                    function {site}{identifier}() {{
                        var imgElement = document.getElementById('{identifier}');
                        imgElement.src = "{path}";
                    }}
                    </script>
        """
    return buttons + initialImage + scripts

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

        buttons += f'<button onclick="{site}{identifier}()" style="padding: 10px; color: white; margin: 4px 4px; background-color: #008CBA;text-transform: uppercase;">{site}</button>'
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
    
def addPlotly(sourceHTML):
	return f"""
            <iframe
                src="resources/{sourceHTML}"
                name="targetframe"
                allowTransparency="true"
                scrolling="no"
                frameborder="0"
                width="700px"
                height="850px"
            >
            </iframe>
			"""
  
def embedMapViewer(srcLink):
    return f'<embed src="{srcLink}" style="width:900px; height: 800px;">'
