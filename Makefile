current_dir = $(shell pwd)

dockerize :
	docker build -t lmullen/gender-predictor .

serve :
	docker run -d -p 3838:3838 \
		-v $(current_dir)/log/:/var/log/ \
		lmullen/gender-predictor
