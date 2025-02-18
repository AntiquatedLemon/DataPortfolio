import pickle
import string
import streamlit as st
import webbrowser

global lrdetect_model
lrdetectfile = open('model.pkl', 'rb')
lrdetect_model = pickle.load(lrdetectfile)
lrdetectfile.close()
st.title("Language Detection Tool")
input_test = st.text_input("Please try me! Put in some text here!", "Hello my name is Monae")

button_clicked = st.button("Get the name of this language")
if button_clicked:
    st.text(lrdetect_model.predict([input_test]))