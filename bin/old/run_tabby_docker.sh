# docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/CodeLlama-7B --device cuda

# docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/StarCoder-7B --device cuda

# docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/StarCoder-3B --device cuda

# docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/SantaCoder-1B --device cuda

# docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/DeepseekCoder-1.3B --device cuda
# docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/DeepseekCoder-6.7B --device cuda

# docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/Mistral-7B --device cuda

docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/CodeGemma-2B --device cuda
# docker run -it --gpus all -p 8080:8080 -v $HOME/.tabby:/data tabbyml/tabby serve --model TabbyML/CodeGemma-7B --device cuda

