from flask import Flask, request, send_from_directory, jsonify
import os
import subprocess
import json
from flask_cors import CORS

app = Flask(__name__)
CORS(app) 

# Define the path to save generated images
IMAGE_DIR = 'static/img/graphs'

@app.route('/generate-graph', methods=['POST'])
def generate_graph():
    data = request.json
    # Save data to a JSON file
    with open('data.json', 'w') as f:
        json.dump(data, f)
    
    # Call the Python script to generate the graph
    subprocess.run(['python3', 'static/python/line_graph.py'], check=True)

    # Return the path to the generated image
    return jsonify({'image_url': '/img/graphs/line_graph.png'})

@app.route('/images/<path:filename>')
def serve_image(filename):
    return send_from_directory(IMAGE_DIR, filename)

if __name__ == '__main__':
    app.run(debug=True)